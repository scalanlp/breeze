package scalanlp.classify

import scalanlp.data.Example
import scalala.tensor.Counter
import scalala.operators._
import bundles.MutableInnerProductSpace
import scalala.generic.math.CanNorm

object Perceptron {
  class Trainer[L,T](maxPasses: Int = 20)(implicit vspace: MutableInnerProductSpace[Double,T], canNorm: CanNorm[T]) extends Classifier.Trainer[L,T] {
    import vspace._
    type MyClassifier = LinearClassifier[L,LFMatrix[L,T],Counter[L,Double],T];
    def train(data: Iterable[Example[L,T]]) = {
      val weights = new LFMatrix[L,T](zeros(data.head.features));
      weights(data.head.label); // seed with one label
      import LFMatrix._
      val result:MyClassifier = new LinearClassifier(weights,Counter[L,Double]());
      for( i <- 0 until maxPasses;  ex <- data) {
        val l = ex.label;
        val feats = ex.features;
        val ctr = result.scores(feats);
        if(ctr.size == 0 || ctr.argmax != l) {
          weights(l) += feats;
          if(ctr.size != 0) {
            weights(ctr.argmax) -= feats;
          }
        }
      }

      result;

    }
  }
}

