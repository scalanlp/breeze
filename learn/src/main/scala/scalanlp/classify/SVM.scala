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



import scalala.Scalala.{sqrt=>_,_};
import scalala.tensor.Tensor1
import scalala.tensor.operators.Tensor1Arith
import scalala.tensor.operators.TensorOp
import scalala.tensor.operators.TensorSelfOp
import scalala.tensor.operators.TensorShapes._;
import scalanlp.util._
import scalanlp.data.Example
import scalanlp.stats.sampling.Rand;
import scalala.tensor.counters.Counters._;
import Math._;


/**
 * Object for creating SupportVectorMachines
 * @author dlwh
 */
object SVM {

  /**
   * Trains an SVM using the Pegsasos Algorithm.
   */
  def apply[F,TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]](data:Seq[Example[Boolean,TF]],numIterations:Int=1000)
                                                               (implicit arith: Tensor1Arith[_,TF,TF,Shape1Col]) = {

    new Pegasos(numIterations).train[F,TF](data);
  }

  /**
   * An online optimizer for an SVM based on Pegasos: Primal Estimated sub-GrAdient SOlver for SVM
   *
   * The optimizer runs a stochastic subgradient descent on the primal objective using
   * batches provided.
   *
   * @author dlwh
   * @param numIterations
   * @param regularization sort of a 2-norm penalty on the weights. Higher means more smoothing
   * @param batchSize: how many elements per iteration to use.
   */
  class Pegasos(numIterations: Int, regularization: Double=0.1, batchSize: Int = 100) extends Logged {
    def train[F,TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]](data: Seq[Example[Boolean,TF]])
                                  (implicit arith: Tensor1Arith[_,TF,TF,Shape1Col]):Classifier[Boolean,TF] = {
      val w = data(0).features.like;
      var intercept = 0.0;
      for(iter <- 0 until numIterations) {
        val subset = (Rand.permutation(data.length).get.take(batchSize)).view.map(data);
        // i.e. those we don't classify correctly
        val problemSubset = (for {
          ex <- subset.iterator
          decision = (w.dot(ex.features) +intercept ) * (if(ex.label) 1 else -1);
          if decision < 1
        } yield ex).toSeq;

        val rate = 1 / (regularization * (iter.toDouble + 1));
        log(Log.INFO)("rate: " + rate);
        log(Log.INFO)("subset size: " + subset.size);
        var w_half = w * (1-rate * regularization) value;
        var bGradient = 0.0;
        problemSubset.foreach { ex =>
          w_half  +=  ex.features * rate / subset.size * (if(ex.label) 1 else -1);
          bGradient +=  (if(ex.label) 1 else -1);
        };
        bGradient /= -problemSubset.size;

        val w_norm = (1 / sqrt(regularization) / norm(w_half,2)) min 1;
        w := w_half * w_norm;
        intercept = (1-rate) * intercept + rate * bGradient;
        log(Log.INFO)("iter: " + iter);
        log(Log.INFO)("weights: " + w.mkString(","));
      }
      new Classifier[Boolean,TF] {
        def scores(f: TF) = {
          val ctr = DoubleCounter[Boolean]();
          ctr(false) = 0.0;
          ctr(true) = w dot f + intercept;
          ctr;
        }
      }
    }
  }

  def main(args: Array[String]) {
    import scalala.Scalala._
    import scalala.tensor.operators.DenseMatrixOps._
    import scalala.Scalala._
    import scalanlp.data._
    import scalala.tensor.dense._;

    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1);
    val vectors = data.rows.map(e => e map ((a:Seq[Double]) => new DenseVector(a.toArray)) relabel (_ == 1.0));

    val trainer = new SVM.Pegasos(10000,batchSize=1000) with ConsoleLogging;
    val classifier = trainer.train[Int,DenseVector](vectors);
    for( ex <- vectors) {
      val guessed = classifier.classify(ex.features);
      println(guessed,ex.label);
    }
  }
}

