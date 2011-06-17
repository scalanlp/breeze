package scalanlp.classify;
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



import scalala.tensor._;

import dense.{DenseVectorCol, DenseVector}
import scalanlp.data._;
import scalanlp.optimize._
import scalala.operators.bundles.MutableInnerProductSpace
import scalala.library.Numerics._
import scalala.library.Library._;
import scalala.generic.math.CanNorm
import scalanlp.util.{ConsoleLogging, I}
;

/**
 * A multi-class logistic/softmax/maxent classifier. It's currently unsmoothed (no regularization)
 * but I hope to fix that at some point.
 *
 * @author dlwh
 */
object LogisticClassifier {


  def main(args: Array[String]) {
    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1);
    val vectors = data.rows.map(e => e map ((a:Seq[Double]) => new DenseVectorCol(a.toArray)) relabel (_.toInt))

    val classifier = new LogisticClassifier.Trainer[Int,DenseVector[Double]].train(vectors);
    for( ex <- vectors) {
      val guessed = classifier.classify(ex.features);
      println(guessed,ex.label);
    }
  }


  /**
   * @param L: the label type
   * @param F: the feature type
   * @param: T2: the Matrix with labels as "rows" and features as "column"
   * @param TL: A "vector" from labels to scores
   * @param TF feature vectors, which are the input vectors to the classifer
   * @param data: a sequence of labeled examples
   * @return a LinearClassifier based on the fitted model
   */
  class Trainer[L,TF]()(implicit arith: MutableInnerProductSpace[Double,TF], canNorm: CanNorm[TF]) extends Classifier.Trainer[L,TF] {
    import arith._;

    type MyClassifier = LinearClassifier[L,LFMatrix[L,TF],Counter[L,Double],TF];

    def train(data: Iterable[Example[L,TF]]) = {
      require(data.size > 0);
      val labelSet = Set.empty ++ data.iterator.map(_.label);
      val allLabels = labelSet.toSeq;

      val guess = new LFMatrix[L,TF](zeros(data.head.features));
      for(l <- labelSet) guess(l) = zeros(data.head.features);

      val obj = objective(data.toIndexedSeq)

      val opt = new LBFGS[LFMatrix[L,TF]](100,7);

      val weights = opt.minimize(obj,guess);
      new LinearClassifier(weights,Counter[L,Double]());
    }


    protected def objective(data: IndexedSeq[Example[L,TF]]) = new ObjectiveFunction(data);

    // preliminaries: an objective function
    protected class ObjectiveFunction(data: IndexedSeq[Example[L,TF]]) extends BatchDiffFunction[LFMatrix[L,TF]] {
      val labelSet = Set.empty ++ data.iterator.map(_.label);
      val allLabels = labelSet.toSeq;

      // Computes the dot product for each label
      def logScores(weights: LFMatrix[L,TF], datum: TF): Counter[L,Double] = {
        weights * datum;
      }

      val fullRange = (0 until data.size)

      override def calculate(weights: LFMatrix[L,TF], range: IndexedSeq[Int]) = {
        var ll = 0.0;
        val grad = weights.empty;

        for( datum <- range.view map data) {
          val logScores = this.logScores(weights,datum.features);
          val logNormalizer = softmax(logScores);
          ll -= (logScores(datum.label) - logNormalizer);
          assert(!ll.isNaN);

          // d ll/d weight_kj = \sum_i x_ij ( I(group_i = k) - p_k(x_i;Beta))
          for ( label <- allLabels ) {
            val prob_k = math.exp(logScores(label) - logNormalizer)
            assert(prob_k >= 0 && prob_k <= 1,prob_k);
            grad(label) -= datum.features * (I(label == datum.label) - prob_k);
          }
        }
        (ll,grad);
      }
    }
  }

}
