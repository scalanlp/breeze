package scalanlp.classify
/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import scalala.library.Library._;
import scalanlp.util._
import scalanlp.data.Example
import scalala.operators._
import bundles.MutableInnerProductSpace
import scalala.tensor._
import scalanlp.stats.sampling.Rand
import scalala.generic.math.CanNorm
import scalala.generic.collection.CanCreateZerosLike


/**
 * Object for creating SupportVectorMachines
 * @author dlwh
 */
object SVM {

  /**
   * Trains an SVM using the Pegsasos Algorithm.
   */
  def apply[L,T](data:Seq[Example[L,T]],numIterations:Int=1000)
              (implicit vspace: MutableInnerProductSpace[Double,T],
               opAssign : BinaryUpdateOp[T,T,OpSet], canNorm: CanNorm[T]):Classifier[L,T] = {

    new Pegasos(numIterations).train(data);
  }

  /**
   * An online optimizer for an SVM based on Pegasos: Primal Estimated sub-GrAdient SOlver for SVM
   * Extended with Wang, Crammer, Vucetic's work for Multiclass
   *
   * The optimizer runs a stochastic subgradient descent on the primal objective using
   * batches provided.
   *
   * @author dlwh
   * @param numIterations
   * @param regularization sort of a 2-norm penalty on the weights. Higher means more smoothing
   * @param batchSize: how many elements per iteration to use.
   */
  class Pegasos[L,T](numIterations: Int,
                   regularization: Double=.1,
                   batchSize: Int = 100)(implicit vspace : MutableInnerProductSpace[Double,T],
                                         opAssign : BinaryUpdateOp[T,T,OpSet], canNorm: CanNorm[T])extends Classifier.Trainer[L,T] with Logged {
    import vspace._;
    type MyClassifier = Classifier[L,T];
    def train(data: Iterable[Example[L,T]]):Classifier[L,T] = {
      val dataSeq = data.toIndexedSeq;
      val default = dataSeq(0).label;

      def guess(w: LFMatrix[L,T], x: T, y: L): (L, Double) = {
        val scores = w * x
        if(scores.size == 0) {
          (default,I(default != y))
        } else {
          val scoreY = scores(y);
          scores(y) = Double.NegativeInfinity
          val r = scores.argmax;
          if(scores(r) + 1 > scoreY) (r,1.0 + scores(r) - scoreY)
          else (y,0.0);
        }
      }

      var w = new LFMatrix[L,T](zeros(dataSeq(0).features));
      for(iter <- 0 until numIterations) {
        val offset = (batchSize * iter) % dataSeq.size
        val subset = (offset until (offset + batchSize)) map (i =>dataSeq(i%dataSeq.size));
        // i.e. those we don't classify correctly
        val problemSubset = (for {
          ex <- subset.iterator
          (r,loss) = guess(w,ex.features,ex.label)
          if r != ex.label
        } yield (ex,r,loss)).toSeq;


        val loss = problemSubset.iterator.map(_._3).sum;

        val rate = 1 / (regularization * (iter.toDouble + 1));
        log(Log.INFO)("rate: " + rate);
        log(Log.INFO)("subset size: " + problemSubset.size);
        val w_half = problemSubset.foldLeft(w * (1-rate * regularization)) { (w,exr) =>
          val (ex,r, oldLoss) = exr
          val et = ex.features * (rate/subset.size);
          w(ex.label) += et
          w(r) -= et
          w
        }


        val w_norm = (1 / (sqrt(regularization) * norm(w_half,2))) min 1;
        w = w_half * w_norm;
        log(Log.INFO)("iter: " + iter + " " + loss);


        val problemSubset2 = (for {
          ex <- subset.iterator
          (r,loss) = guess(w,ex.features,ex.label)
          if r != ex.label
        } yield (ex,r,loss)).toSeq;


        val loss2 = problemSubset2.iterator.map(_._3).sum;
        log(Log.INFO)("Post loss: " + loss2);
      }
      new LinearClassifier(w,Counter[L,Double]());
    }
  }

  def main(args: Array[String]) {
    import scalanlp.data._
    import scalala.tensor.dense._;

    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1,dropRow = true);
    var vectors = data.rows.map(e => e map ((a:Seq[Double]) => DenseVector(a:_*)) relabel (_.toInt));
    vectors = vectors.map { _.map{ v2 => v2 /norm(v2,2) }};
    vectors = Rand.permutation(vectors.length).draw.map(vectors) take 10;

    println(vectors.length);

    val trainer = new SVM.Pegasos[Int,DenseVector[Double]](100,batchSize=1000) with ConsoleLogging;
    val classifier = trainer.train(vectors);
    for( ex <- vectors.take(30)) {
      val guessed = classifier.classify(ex.features);
      println(guessed,ex.label);
    }
  }

  class FobosTrainer()
}

