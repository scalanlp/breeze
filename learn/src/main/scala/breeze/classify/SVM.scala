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



import chalk.data.Example
import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Rand
import breeze.math.MutableCoordinateSpace
import breeze.util.Index
import com.typesafe.scalalogging.log4j.Logging
import scala.reflect.ClassTag


/**
 * Object for creating SupportVectorMachines
 * @author dlwh
 */
object SVM {

  /**
   * Trains an SVM using the Pegasos Algorithm.
   */
  def apply[L,T](data:Seq[Example[L,T]],numIterations:Int=1000)
              (implicit vspace: MutableCoordinateSpace[T, Double],
                man: ClassTag[T]):Classifier[L,T] = {

    new SMOTrainer(numIterations).train(data)
  }

  /**
   * An online optimizer for an SVM based on Pegasos: Primal Estimated sub-GrAdient SOlver for SVM
   * Extended with Wang, Crammer, Vucetic's work for Multiclass
   *
   * The optimizer runs a stochastic subgradient descent on the primal objective using
   * batches provided.
   *
   * @author dlwh
   * @param numIterations number of passes through the dataset
   * @param regularization sort of a 2-norm penalty on the weights. Higher means more smoothing
   * @param batchSize: how many elements per minibatch to use.
  class Pegasos[L,T](numIterations: Int,
                    regularization: Double=.1,
                    batchSize: Int = 100)(implicit vspace : MutableCoordinateSpace[T, Double])extends Classifier.Trainer[L,T] with Logged with ConfiguredLogging {
     import vspace._
     type MyClassifier = LinearClassifier[L,LFMatrix[L,T],Counter[L,Double],T]
     def train(data: Iterable[Example[L,T]]) = {
       val dataSeq = data.toIndexedSeq
       val default = dataSeq(0).label

       def guess(w: LFMatrix[L,T], x: T, y: L): (L, Double) = {
         val scores = w * x
         println(scores, y)
         if(scores.size == 0) {
           (default,I(default != y))
         } else {
           val scoreY = scores(y)
           scores(y) = Double.NegativeInfinity
           val r = scores.argmax
           if(scores(r) + 1 > scoreY) (r,1.0 + scores(r) - scoreY)
           else (y,0.0)
         }
       }

       var w = new LFMatrix[L,T](zeros(dataSeq(0).features))
       // force one to show up
       w(default)
       for(iter <- 0 until (numIterations * dataSeq.size/batchSize)) {
         val offset = (batchSize * iter) % dataSeq.size
         val subset = (offset until (offset + batchSize)) map (i =>dataSeq(i%dataSeq.size))
         // i.e. those we don't classify correctly
         val problemSubset = (for {
           ex <- subset.iterator
           (r,loss) = guess(w,ex.features,ex.label)
           if r != ex.label
         } yield (ex,r,loss)).toSeq


         val loss = problemSubset.iterator.map(_._3).sum

         val rate = 1 / (regularization * (iter.toDouble + 1))
         logger.info("rate: " + rate)
         logger.info("subset size: " + problemSubset.size)
         val w_half = problemSubset.foldLeft(w * (1-rate * regularization)) { (w,exr) =>
           val (ex,r, oldLoss) = exr
           val et = ex.features * (rate/subset.size)
           w(ex.label) += et
           w(r) -= et
           w
         }


         val w_norm = (1 / (sqrt(regularization) * breeze.linalg.norm(w_half,2))) min 1
         w = w_half * w_norm
         println(w, w_half)
         logger.info("iter: " + iter + " " + loss)


         val problemSubset2 = (for {
           ex <- subset.iterator
           (r,loss) = guess(w,ex.features,ex.label)
           if r != ex.label
         } yield (ex,r,loss)).toSeq


         val loss2 = problemSubset2.iterator.map(_._3).sum
         logger.info("Post loss: " + loss2)
       }
       new LinearClassifier(w,Counter[L,Double]())
     }
   }

   */

  def main(args: Array[String]) {
    import chalk.data._

    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1,dropRow = true)
    var vectors = data.rows.map(e => e map ((a:Seq[Double]) => DenseVector(a:_*)) relabel (_.toInt))
    vectors = vectors.map { _.map{ v2 => v2 /norm(v2,2) }}
    vectors = Rand.permutation(vectors.length).draw.map(vectors) take 10

    println(vectors.length)

    val trainer = new SVM.SMOTrainer[Int,DenseVector[Double]](100) with Logging
    val classifier = trainer.train(vectors)
    for( ex <- vectors.take(30)) {
      val guessed = classifier.classify(ex.features)
      println(guessed,ex.label)
    }
  }

  class SMOTrainer[L,T](maxIterations: Int=30,
                        C: Double = 10.0)
                       (implicit vspace: MutableCoordinateSpace[T, Double],
                        man: ClassTag[T]) extends Classifier.Trainer[L,T] with Logging {
    type MyClassifier = LinearClassifier[L,UnindexedLFMatrix[L,T],Counter[L,Double],T]

    import vspace._

    def train(data: Iterable[Example[L, T]]) = {
      val alphas = data.map { d => Counter[L,Double](d.label -> C)}.toArray
      val labelIndex = Index[L](data.map(_.label))
      val weights = new LFMatrix[L,T](zeros(data.head.features), labelIndex)
      val allLabels = data.iterator.map(_.label).toSet
      weights(data.head.label); // seed with one label
      var largestChange = 10000.0
      for(iter <- 0 until maxIterations if largestChange > 1E-4) {
        largestChange = 0.0
        for{
          (d,alpha) <- data zip alphas
          label1 <- allLabels
          label2 <- allLabels
        } {
          val oldA1 = alpha(label1)
          val oldA2 = alpha(label2)
          val loss1 = I(label1 != d.label)
          val loss2 = I(label2 != d.label)
          val feats = d.features
          var t = ((loss1 - loss2) - (weights(label2).dot(feats) - weights(label1).dot(feats))) / (2 * feats.dot(feats))
          if(!t.isNaN && t != 0.0) {
            t = t max (-oldA1)
            val newA1 = (oldA1 + t) min (oldA1 + oldA2)
            val newA2 = (oldA2 - t) max 0
            alpha(label1) = newA1
            alpha(label2) = newA2
            weights(label1) += feats * (oldA1 - newA1)
            weights(label2) += feats * (oldA2 - newA2)
            largestChange = largestChange max (oldA1 - newA1).abs
            largestChange = largestChange max (oldA2 - newA2).abs
          }
        }
        logger.info("Largest Change: " + largestChange)

      }
      new LinearClassifier(weights.unindexed,Counter[L,Double]())
    }
  }

}



