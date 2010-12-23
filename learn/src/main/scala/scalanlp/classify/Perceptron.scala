package scalanlp.classify

import scalala.tensor.Tensor1
import scalala.tensor.operators.TensorSelfOp
import scalala.tensor.operators.TensorShapes._
import scalanlp.data.Example
;
import scalala.tensor.counters.Counters._;


/**
 *
 * @author dlwh
 */
@serializable
trait Perceptron[L,-T] extends Classifier[L,T];


object Perceptron {
  class Trainer[L,F,T<:Tensor1[F] with TensorSelfOp[F,T,Shape1Col]] extends Classifier.Trainer[L,T] {
    type MyClassifier = Perceptron[L,T];
    def train(it: Iterable[Example[L,T]]) = {
      val weights = new collection.mutable.HashMap[L,T]();
      val result = new Perceptron[L,T] {
        def scores(o: T) = {
          val r = DoubleCounter[L];
          for((l,w) <- weights) {
            r(l) = w dot o;
          }
          r
        }
      };
      for( ex <- it) {
        val l = ex.label;
        val feats = ex.features;
        val ctr = result.scores(feats);
        if(ctr.size == 0 || ctr.argmax != l) {
          weights.getOrElseUpdate(l,feats.like) += feats;
          if(ctr.size != 0) {
            weights.getOrElseUpdate(ctr.argmax,feats.like) -= feats;
          }
        }
        println(weights)
      }

      result;

    }
  }
}

