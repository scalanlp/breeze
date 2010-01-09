package scalanlp.classify

import scalala.Scalala.{sqrt=>_,iSeqToDoublePartialMap=>_,_};
import scalala.tensor.Tensor1
import scalala.tensor.operators.Tensor1Arith
import scalala.tensor.operators.TensorOp
import scalala.tensor.operators.TensorSelfOp
import scalala.tensor.operators.TensorShapes._;
import scalanlp.util.Logged
import scalanlp.data.Example
import scalanlp.util.Implicits.SeqExtras;
import scalanlp.stats.sampling.Rand;
import Math._;


object SVM {
  class Pegasos(numIterations: Int, regularization: Double=0.1, batchSize: Int = 100) extends Logged {
    def train[F,TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]](data: Seq[Example[Boolean,TF]])
                                  (implicit arith: Tensor1Arith[_,TF,TF,Shape1Col]) = {
      val w = data(0).features.like;
      // Suggested in the pegasos paper.
      val learningRateMultiplier = 0.5 * sqrt(2 + 0.5 / sqrt(numIterations) );
      for(iter <- 0 until numIterations) {
        val subset = data(Rand.permutation(data.length).get.take(batchSize));
        val problemSubset = for {
          ex <- subset
          decision = w.dot(ex.features) * (if(ex.label) 1 else -1);
          if decision < 1
        } yield ex;

        val rate = learningRateMultiplier / (regularization * sqrt(iter.toDouble));
        val w_half = problemSubset.foldLeft( w * (1-rate * regularization): TensorOp[TF,Shape1Col]) { (acc,ex)=>
          acc +  ex.features * rate / subset.size * (if(ex.label) 1 else -1);
        };

        val w_norm = norm(w_half,2) / sqrt(regularization);
        if(w_norm > 1)
          w := w_half * w_norm;
      }
    }
  }
}

