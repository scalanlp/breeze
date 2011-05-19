package scalanlp.classify

import scalanlp.data.Example
import scalala.tensor.Counter
import scalala.generic.collection.CanCreateZerosLike
import scalala.operators._

/**
 *
 * @author dlwh
 */
@serializable
trait Perceptron[L,-T] extends Classifier[L,T];


object Perceptron {
  class Trainer[L,T](maxPasses: Int = 20)(implicit dotProduct: BinaryOp[T,T,OpMulInner,Double],
                       zeros: CanCreateZerosLike[T,T],
                       numeric: T=>MutableNumericOps[T],
                       upAdd: BinaryUpdateOp[T,T,OpAdd],
                       upSub: BinaryUpdateOp[T,T,OpSub]) extends Classifier.Trainer[L,T] {
    type MyClassifier = Perceptron[L,T];
    def train(it: Iterable[Example[L,T]]) = {
      val weights = new collection.mutable.HashMap[L,T]();
      val result = new Perceptron[L,T] {
        def scores(o: T) = {
          val r = Counter[L,Double];
          for((l,w) <- weights) {
            r(l) = w dot o;
          }
          r
        }
      };
      for( i <- 0 until maxPasses;  ex <- it) {
        val l = ex.label;
        val feats = ex.features;
        val ctr = result.scores(feats);
        if(ctr.size == 0 || ctr.argmax != l) {
          weights.getOrElseUpdate(l,zeros(feats)) += feats;
          if(ctr.size != 0) {
            weights.getOrElseUpdate(ctr.argmax,zeros(feats)) -= feats;
          }
        }
      }

      result;

    }
  }
}

