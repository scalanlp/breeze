package breeze.classify
/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import breeze.linalg._
import breeze.numerics._
import breeze.data._
import breeze.optimize._
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.math.MutableCoordinateSpace

/**
 * A multi-class logistic/softmax/maxent classifier. It's currently unsmoothed (no regularization)
 * but I hope to fix that at some point.
 *
 * @author dlwh
 */
object LogisticClassifier {


  def main(args: Array[String]) {
    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1)
    val vectors = data.rows.map(e => e map ((a:Seq[Double]) => new DenseVector(a.toArray)) relabel (_.toInt))

    val classifier = new LogisticClassifier.Trainer[Int,DenseVector[Double]].train(vectors)
    for( ex <- vectors) {
      val guessed = classifier.classify(ex.features)
      println(guessed,ex.label)
    }
  }


  /**
   * @tparam L: the label type
   * @tparam TF feature vectors, which are the input vectors to the classifer
   * @return a LinearClassifier based on the fitted model
   */
  class Trainer[L,TF](opt: OptParams = OptParams())(implicit arith: MutableCoordinateSpace[TF, Double]) extends Classifier.Trainer[L,TF] {
    import arith._

    type MyClassifier = LinearClassifier[L,LFMatrix[L,TF],Counter[L,Double],TF]

    def train(data: Iterable[Example[L,TF]]) = {
      require(data.size > 0)
      val labelSet = Set.empty ++ data.iterator.map(_.label)

      val guess = new LFMatrix[L,TF](zeros(data.head.features))
      for(l <- labelSet) guess(l) = zeros(data.head.features)

      val obj = new CachedBatchDiffFunction(objective(data.toIndexedSeq))(LFMatrix.canCopy(copy))

      val weights = opt.minimize(obj,guess)(LFMatrix.coordSpace)
//
//      val weights = new LBFGS[LFMatrix[L, TF]]().minimize(obj, guess)

      new LinearClassifier(weights,Counter[L,Double]())
    }


    protected def objective(data: IndexedSeq[Example[L,TF]]) = new ObjectiveFunction(data)

    // preliminaries: an objective function
    protected class ObjectiveFunction(data: IndexedSeq[Example[L,TF]]) extends BatchDiffFunction[LFMatrix[L,TF]] {
      val labelSet = Set.empty ++ data.iterator.map(_.label)
      val allLabels = labelSet.toSeq

      // Computes the dot product for each label
      def logScores(weights: LFMatrix[L,TF], datum: TF): Counter[L,Double] = {
        weights * datum
      }

      val fullRange = (0 until data.size)

      override def calculate(weights: LFMatrix[L,TF], range: IndexedSeq[Int]) = {
        var ll = 0.0
        val grad = weights.empty
        assert(!breeze.linalg.norm(weights).isNaN, weights)

        for( datum <- range.view map data) {
          val logScores = this.logScores(weights,datum.features)
          val logNormalizer = softmax(logScores)
          ll -= (logScores(datum.label) - logNormalizer)
          assert(!ll.isNaN, logNormalizer + " " + logScores + " " + weights + " " + datum + " " + (weights * datum.features))

          // d ll/d weight_kj = \sum_i x_ij ( I(group_i = k) - p_k(x_i;Beta))
          for ( label <- allLabels ) {
            val prob_k = math.exp(logScores(label) - logNormalizer)
            assert(prob_k >= 0 && prob_k <= 1,prob_k)
            grad(label) -= datum.features * (I(label == datum.label) - prob_k)
          }
        }
        assert(!breeze.linalg.norm(grad).isNaN, grad)
        (ll,grad)
      }
    }
  }

}


/**
 * This is an example app for creating a logistic classifier from data that is 
 * stored as string valued features and string valued labels, e.g.
 * 
 * verb=join,noun=board,prep=as,prep_obj=director,V
 * verb=is,noun=chairman,prep=of,prep_obj=N.V.,N
 * verb=named,noun=director,prep=of,prep_obj=conglomerate,N
 *
 * These are examples from Ratnarparkhi's classic prepositional phrase attachment
 * dataset, discussed in the following homework:
 *
 *   http://ata-s12.utcompling.com/assignments/classification
 *
 * The homework includes pointers to the data and to Scala code for generating
 * said features.
 *
 * This example handles creating a feature index and getting the examples into the
 * right data structures for training with the logistic regression classifier,
 * which should serve as a useful example for creating features and classifiers
 * using the API.
 * 
 * @author jasonbaldridge
 */ 
object LogisticClassifierFromCsv {

  import breeze.util.Index
  import breeze.config._
  import breeze.stats.ContingencyStats
  import Counter._
  import java.io.File

  case class Params(
    @Help(text="Input training file in CSV format.") train: File,
    @Help(text="Input eval file in CSV format.") eval: File,
    @Help(text="Show evaluation.") showEval: Boolean = false,
    @Help(text="Output full distributions.") fullOutput: Boolean = false,
    @Help(text="Regularization value (default 1.0).") reg: Double = 1.0,
    @Help(text="Tolerance (stopping criteria) (default 1E-4).") tol: Double = 1E-4,
    @Help(text="Maximum number of iterations (default 1000).") maxIterations: Int = 1000,
    @Help(text="Prints this") help:Boolean = false
  )

  def main(args: Array[String]) {

    val config = CommandLineParser.parseArguments(args)._1
    val params = config.readIn[Params]("")

    // Feature map
    val fmap = Index[String]()

    // Read in the training data and index it.
    val trainingData = 
      SparseCsvDataset(io.Source.fromFile(params.train))
        .map(ex => ex.map(_.map(fmap.index(_))))
        .toList // Need to consume the lines in order to populate the feature map

    val numFeatures = fmap.size

    val trainingExamples = 
      trainingData
        .zipWithIndex
        .map { case(ex, rowId) => {
          Example[String,SparseVector[Double]](
            label=ex.label,
            features=makeSparseFeatureVector(ex, numFeatures),
            id=rowId.toString
          )
        }}

    // Train the classifier
    val opt = OptParams(
      regularization=params.reg,
      maxIterations = params.maxIterations,
      tolerance = params.tol
    )

    val classifier = 
      new LogisticClassifier.Trainer[String,SparseVector[Double]](opt)
        .train(trainingExamples)

    // Read in the evaluation data
    val evalExamples = 
      SparseCsvDataset(io.Source.fromFile(params.eval))
        .zipWithIndex
        .map { case(ex, rowId) => {
          val mappedEx = ex.map(_.flatMap(fmap.indexOpt(_)))
          Example[String, SparseVector[Double]](
            label=ex.label,
            features=makeSparseFeatureVector(mappedEx, numFeatures),
            id=rowId.toString
          )
        }}
        .toList

    // Get the predictions
    val predictions = evalExamples.map(ex => classifier.scores(ex.features))

    // Output full predictions
    if (params.fullOutput) {
      predictions.foreach { prediction => {
        val pcounter = prediction.asInstanceOf[Counter[String,Double]]
        val distribution = logNormalize(pcounter).mapValues(math.exp(_))
        val sortedDistributionString =
          distribution
            .argsort
            .reverse
            .map(label => label + " " + distribution(label)).mkString(" ")
        println(sortedDistributionString)
      }}
    }

    // Output evaluation statistics
    if (params.showEval) {
      println("\n-----------------------------------------------------")
      println("Training set 'performance':")
      println(ContingencyStats(classifier,trainingExamples))
      
      println("\n-----------------------------------------------------")
      println("Eval set performance:")
      println(ContingencyStats(classifier, evalExamples))
    }

  }

  /**
   * Creates a sparse feature vector from an indexed example. Assumes we have
   * observations of each feature, and these could show up multiple times, so
   * we count those occurences and use those as the values stored in the
   * vector.
   */
  private def makeSparseFeatureVector (example: Example[String, Seq[Int]], numFeatures: Int) = {
    val vec = SparseVector.zeros[Double](numFeatures)
    example
      .features
      .groupBy(x=>x)
      .mapValues(_.length.toDouble)
      .foreach { case(index, value) => vec(index) = value }
    vec
  }


  /**
   * Read in a dataset and create Examples from it. Don't do any feature indexation,
   * since for training data we want to build the index, but for eval data we just
   * want to use it.
   */
  object SparseCsvDataset {
  
    def apply(dataSource: io.Source): Iterator[Example[String, Seq[String]]] =
      dataSource
        .getLines
        .zipWithIndex
        .map { case(line,rowId) => { 
          val lineData = line.split(",")
          val (features, label) = (lineData.dropRight(1), lineData.last);
          Example[String, Seq[String]](label=label, features=features, id=rowId.toString)
        }}

  }

}


