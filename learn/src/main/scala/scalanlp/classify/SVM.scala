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
  def apply[T](data:Seq[Example[Boolean,T]],numIterations:Int=1000)
              (implicit view : T=>MutableNumericOps[T],
               add : BinaryOp[T,T,OpAdd,T],
               addScalar : BinaryOp[T,Double,OpAdd,T],
               upAdd : BinaryUpdateOp[T,T,OpAdd],
               mulScalar : BinaryOp[T,Double,OpMul,T],
               mulScalarInto : BinaryUpdateOp[T,Double,OpMul],
               opAssign : BinaryUpdateOp[T,T,OpSet],
               hasInnerProduct: BinaryOp[T,T,OpMulInner,Double],
               canNorm: CanNorm[T],
               zeros: CanCreateZerosLike[T,T]):Classifier[Boolean,T] = {

    new Pegasos(numIterations).train(data);
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
  class Pegasos[T](numIterations: Int,
                   regularization: Double=0.1,
                   batchSize: Int = 100)(implicit view : T=>MutableNumericOps[T],
                                         add : BinaryOp[T,T,OpAdd,T],
                                         addScalar : BinaryOp[T,Double,OpAdd,T],
                                         upAdd : BinaryUpdateOp[T,T,OpAdd],
                                         mulScalar : BinaryOp[T,Double,OpMul,T],
                                         mulScalarInto : BinaryUpdateOp[T,Double,OpMul],
                                         opAssign : BinaryUpdateOp[T,T,OpSet],
                                         hasInnerProduct: BinaryOp[T,T,OpMulInner,Double],
                                         canNorm: CanNorm[T],
                                         zeros: CanCreateZerosLike[T,T])extends Classifier.Trainer[Boolean,T] with Logged {
    type MyClassifier = Classifier[Boolean,T];
    def train(data: Iterable[Example[Boolean,T]]):Classifier[Boolean,T] = {
      val dataSeq = data.toIndexedSeq;
      val w = zeros(dataSeq(0).features);
      var intercept = 0.0;
      for(iter <- 0 until numIterations) {
        val subset = (Rand.permutation(dataSeq.size).get.take(batchSize)).view.map(dataSeq);
        // i.e. those we don't classify correctly
        val problemSubset = (for {
          ex <- subset.iterator
          decision = (w.dot(ex.features) +intercept ) * (if(ex.label) 1 else -1);
          if decision < 1
        } yield ex).toSeq;

        val rate = 1 / (regularization * (iter.toDouble + 1));
        log(Log.INFO)("rate: " + rate);
        log(Log.INFO)("subset size: " + subset.size);
        var w_half = w * (1-rate * regularization);
        var bGradient = 0.0;
        problemSubset.foreach { ex =>
          w_half  +=  ex.features * rate * ((if(ex.label) 1.0 else -1.0) / subset.size);
          bGradient +=  (if(ex.label) 1 else -1);
        };
        bGradient /= -problemSubset.size;

        val w_norm = (1 / sqrt(regularization) / norm(w_half,2)) min 1;
        w := w_half * w_norm;
        intercept = (1-rate) * intercept + rate * bGradient;
        log(Log.INFO)("iter: " + iter);
        log(Log.INFO)("weights: " + w);
      }
      new Classifier[Boolean,T] {
        def scores(f: T) = {
          val ctr = Counter[Boolean,Double]();
          ctr(false) = 0.0;
          ctr(true) = w dot f + intercept;
          ctr;
        }
      }
    }
  }

  def main(args: Array[String]) {
    import scalanlp.data._
    import scalala.tensor.dense._;

    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1);
    val vectors = data.rows.map(e => e map ((a:Seq[Double]) => DenseVector(a:_*)) relabel (_ == 1.0));

    val trainer = new SVM.Pegasos[DenseVector[Double]](10000,batchSize=1000) with ConsoleLogging;
    val classifier = trainer.train(vectors);
    for( ex <- vectors) {
      val guessed = classifier.classify(ex.features);
      println(guessed,ex.label);
    }
  }
}

