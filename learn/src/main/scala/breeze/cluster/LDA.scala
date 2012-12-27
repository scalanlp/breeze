package breeze.cluster

import breeze.linalg._
import breeze.numerics._
import breeze.config.CommandLineParser
import breeze.util.Index
import java.io.File
import breeze.text.tokenize.JavaWordTokenizer
import breeze.text.transform.StopWordFilter
import scala.io._
import breeze.util.Implicits._

/**
 * 
 * @author dlwh
 */
class LDA(numTopics: Int, topicSmoothing: Double = 0.5, wordSmoothing: Double = 0.05, numIterations: Int = 50) {
  import LDA._


  def fitModel(data: IndexedSeq[SparseVector[Double]]): Model = iterations(data).drop(1).take(numIterations).last

  def iterations(data: IndexedSeq[SparseVector[Double]]): Iterator[Model] = {
    val numWords = data.head.size
    val termWeights = DenseMatrix.rand(numTopics, numWords) / numWords.toDouble

    Iterator.iterate(Model(termWeights, 0.0, numTopics, topicSmoothing, wordSmoothing)) { current =>
      var ll = 0.0
      val counts = data.par.aggregate(null:DenseMatrix[Double])({ ( _counts, doc) =>
        val counts =  Option(_counts).getOrElse(DenseMatrix.zeros[Double](numTopics, numWords))
        val result = current.inference(doc)
        val gamma = result.wordLoadings
        // sum up expected counts
        var i = 0
        ll += result.ll
        while(i < doc.activeSize) {
          counts(::, doc.indexAt(i)) += gamma(::, i) * doc.valueAt(i)
          i += 1
        }
        counts
      }, {_ += _})

      // m step: Beta = exp(digamma(counts) - digamma(\sum(counts))
      counts += topicSmoothing
      val newCounts =  digamma(counts)
      for(k <- 0 until numTopics) {
        newCounts(k, ::) -= digamma(sum(counts(k,::)))
      }

      // compute the rest of the likelihood (from the word counts)
      ll += numTopics * (lgamma(wordSmoothing * numWords) - numWords * lgamma(wordSmoothing))
      ll += (wordSmoothing-1) * (newCounts.sum)
      ll -= ((counts - 1.0) :* (newCounts)).sum
      ll += lbeta(counts, Axis._1).sum

      exp.inPlace(newCounts)
      current.copy(newCounts, ll)
    }

  }.drop(1).take(numIterations)
}

object LDA {

  case class Model(termWeights: DenseMatrix[Double], likelihood: Double, numTopics: Int, topicSmoothing: Double, wordSmoothing: Double) {
    case class InferenceResult(topicLoadings: DenseVector[Double], wordLoadings: DenseMatrix[Double], ll: Double)
    def inference(doc: SparseVector[Double]) = {
      var converged = false
      var iter = 25
      val gamma = DenseMatrix.zeros[Double](numTopics, doc.activeSize)
      gamma := topicSmoothing
      var alpha = DenseVector.fill(numTopics)(topicSmoothing)
      var newAlpha = DenseVector.zeros[Double](numTopics)
      var ll = 0.0

      // inference
      while (!converged && iter > 0) {
        converged = true
        newAlpha := topicSmoothing
        iter -= 1
        var i = 0
        while (i < doc.activeSize) {
          val result = normalize(alpha :* termWeights(::, doc.indexAt(i)), 1)
          assert(!norm(result).isNaN, gamma(::, i).toString + " " + alpha.toString + " " + termWeights(::, doc.indexAt(i)))

          converged &&= norm(gamma(::, i) - result, Double.PositiveInfinity) < 1E-4
          gamma(::, i) := result
          newAlpha += (result * doc.valueAt(i))
          i += 1
        }
        val newLL = likelihood(doc, newAlpha, gamma)
        ll = newLL

        if (!converged) {
          val xx = newAlpha
          newAlpha = alpha
          alpha = xx
          digamma.inPlace(alpha)
          exp.inPlace(alpha)
        }

      }
      InferenceResult(alpha, gamma, ll)
    }

    private def likelihood(doc: SparseVector[Double], theta: DenseVector[Double], gamma: DenseMatrix[Double]) = {
      val dig = digamma(theta)
      val digsum = digamma(sum(theta))
      var ll = lgamma(topicSmoothing * numTopics) - numTopics * lgamma(topicSmoothing) - lgamma(sum(theta))
      var k = 0
      while(k < numTopics) {
        ll +=  (topicSmoothing - 1)*(dig(k) - digsum) + lgamma(theta(k)) - (theta(k) - 1)*(dig(k) - digsum)
        var i = 0
        while(i < doc.activeSize) {
          val n = doc.indexAt(i)
          ll += doc.valueAt(i) * (gamma(k, i)*((dig(k) - digsum) - log(gamma(k, i)) + math.log(termWeights(k, n))))
          i += 1
        }

        k += 1
      }

      ll
    }
  }

  case class Params(dir: File,
                    numTopics: Int = 20,
                    topicSmoothing: Double = .1,
                    wordSmoothing: Double = 0.1)
  def main(args: Array[String]) = {
    val config = CommandLineParser.parseArguments(args)._1
    val params = config.readIn[Params]("")
    import params._

    // Feature map
    val fmap = Index[String]()


    val removeStopWords = new StopWordFilter("en")
    // Read in the training data and index it.
    val almostTrainingData = for {
      f <- dir.listFiles
    } yield {
      val text = Source.fromFile(f)("UTF-8").mkString
      val builder = new VectorBuilder[Double](Int.MaxValue, text.length / 20)
      for(tok <- JavaWordTokenizer(text) if tok(0).isLetter && removeStopWords(tok)) {
        builder.add(fmap.index(tok), 1.0)
      }

      builder
    }

    val trainingData = almostTrainingData.map{ b => b.length = fmap.size; b.toSparseVector}

    val lda = new LDA(params.numTopics, params.topicSmoothing, params.wordSmoothing)

    val model = lda.iterations(trainingData).tee(m => println(m.likelihood)).last
    val topKLists = for(k <- 0 until numTopics) yield model.termWeights.t(::, k).argtopk(50).map(i => fmap.get(i) + " "+  model.termWeights(k, i))
    for( (list,k) <- topKLists.zipWithIndex) {
      println("Topic %d:".format(k))
      println(list.mkString("\t","\n\t", "\n"))
    }

  }
}