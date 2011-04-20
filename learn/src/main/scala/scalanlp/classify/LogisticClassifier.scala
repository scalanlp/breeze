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

import scalanlp.data._;
import scalanlp.optimize._;

/**
 * A multi-class logistic/softmax/maxent classifier. It's currently unsmoothed (no regularization)
 * but I hope to fix that at some point.
 *
 * @author dlwh
object LogisticClassifier {


  def main(args: Array[String]) {
    import scalala.Scalala._
    import scalala.tensor.operators.DenseMatrixOps._
    import scalala.Scalala._
    import scalanlp.data._
    import scalala.tensor.dense._;                                                                                           

    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1);
    val vectors = data.rows.map(e => e map ((a:Seq[Double]) => new DenseVector(a.toArray)) relabel (_.toInt))

    val classifier = new LogisticClassifier.Trainer[Int,Int,DenseMatrix,DenseVector,DenseVector].train(vectors);
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
  class Trainer[L,F,T2<:Tensor2[L,F] with TensorSelfOp[(L,F),T2,Shape2],
      TL<:Tensor1[L] with TensorSelfOp[L,TL,Shape1Col],
      TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]]
      (implicit tpb: TensorProductBuilder[T2,TF,TL,Shape2,Shape1Col,Shape1Col],
        ta: TensorArith[(L,F),T2,Tensor2[L,F],Shape2],
        tla: Tensor1Arith[L,TL,TL,Shape1Col],
        datasetModel: DatasetModel[L,F,T2,TL,TF]) extends Classifier.Trainer[L,TF] {

    type MyClassifier = LinearClassifier[L,F,T2,TL,TF];

    protected val linearizer = TensorLinearizer[(L,F),T2]();
    import linearizer._;

    def train(data: Iterable[Example[L,TF]]) = {
      require(data.size > 0);

      val guess = datasetModel.emptyParameterMatrix(data.toSeq);

      val obj = objective(data.toIndexedSeq);

      val flatWeights = opt.minimize(obj,linearize(guess));
      val weights = reshape(flatWeights);
      new LinearClassifier[L,F,T2,TL,TF](weights,datasetModel.emptyLabelVector(data.toSeq));
    }


    protected def objective(data: IndexedSeq[Example[L,TF]]) = new ObjectiveFunction(data);
    protected val opt:Minimizer[ProjectedTensor,BatchDiffFunction[(L,F),ProjectedTensor]] = {
      new LBFGS[(L,F),ProjectedTensor](-1,5)(ops) with scalanlp.util.ConsoleLogging;
    }

    // preliminaries: an objective function
    protected class ObjectiveFunction(data: IndexedSeq[Example[L,TF]]) extends BatchDiffFunction[(L,F),ProjectedTensor] {

      val basisLabel = data.head.label;
      val labelSet = Set.empty ++ data.iterator.map(_.label);
      val allLabels = labelSet.toSeq;

      // Computes the dot product for each label
      def logScores(weights: T2, datum: TF) = {
        val logScores = Map.empty ++ (for(label <- allLabels)
                            yield (label,weights.getRow(label) dot datum));
        logScores;
      }



      val fullRange = (0 until data.size)

      override def calculate(flatWeights: ProjectedTensor, range: IndexedSeq[Int]) = {
        var ll = 0.0;
        val weights = reshape(flatWeights);
        val grad = weights.like;

        for( datum <- range.view map data) {
          val logScores = this.logScores(weights,datum.features);
          val logNormalizer = logSum(logScores.valuesIterator.toSeq);
          ll -= (logScores(datum.label) - logNormalizer);
          assert(!ll.isNaN);

          // d ll/d weight_kj = \sum_i x_ij ( I(group_i = k) - p_k(x_i;Beta))
          for {
            label <- allLabels
            if label != basisLabel
            prob_k = math.exp(logScores(label) - logNormalizer)
            () = assert(prob_k >= 0 && prob_k <= 1,prob_k);
            shift = if(label == datum.label) (1-prob_k) else -prob_k
            (feat,featValue) <- datum.features
          } {
            grad(label,feat) -= featValue * shift;
          }
        }
        (ll,linearize(grad));
      }
    }
  }

}
*/
