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
import scalala.tensor.operators.TensorShapes._;
import scalala.tensor.operators._;

import scalanlp.data._;
import scalanlp.math.Numerics._;
import scalanlp.optimize._;

/**
 * A multi-class logistic/softmax/maxent classifier. It's currently unsmoothed (no regularization)
 * but I hope to fix that at some point.
 *
 * @author dlwh
 */
object LogisticClassifier {
  /**
   * @param L: the label type
   * @param F: the feature type
   * @param: T2: the Matrix with labels as "rows" and features as "column"
   * @param TL: A "vector" from labels to scores
   * @param TF feature vectors, which are the input vectors to the classifer
   * @param data: a sequence of labeled examples
   * @return a LinearClassifier based on the fitted model
   */
  def apply[L,F,T2<:Tensor2[L,F] with TensorSelfOp[(L,F),T2,Shape2],
      TL<:Tensor1[L] with TensorSelfOp[L,TL,Shape1Col],
      TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]]
      (data: Seq[Example[L,TF]])
      (implicit tpb: TensorProductBuilder[T2,TF,TL,Shape2,Shape1Col,Shape1Col],
        ta: TensorArith[(L,F),T2,Tensor2[L,F],Shape2],
        tla: Tensor1Arith[L,TL,TL,Shape1Col],
        datasetModel: DatasetModel[L,F,T2,TL,TF]) = {

    require(data.length > 0);

    val guess = datasetModel.emptyParameterMatrix(data);

    val maker = new ObjectiveFunctionMaker[L,F,T2,TL,TF];
    import maker.linearizer._;
    val objective = maker.objective(data);

    val opt = new LBFGS[(L,F),ProjectedTensor](-1,5)(maker.linearizer.ops) with scalanlp.util.ConsoleLogging;
    //val opt = new StochasticGradientDescent[(L,F),ProjectedTensor](0.05,0,100000) with scalanlp.util.ConsoleLogging;
    val flatWeights = opt.minimize(objective,linearize(guess));
    val weights = reshape(flatWeights);
    new LinearClassifier[L,F,T2,TL,TF](weights,datasetModel.emptyLabelVector(data));
  }

  class ObjectiveFunctionMaker[L,F,T2<:Tensor2[L,F] with TensorSelfOp[(L,F),T2,Shape2],
      TL<:Tensor1[L] with TensorSelfOp[L,TL,Shape1Col],
      TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]]
      (implicit tpb: TensorProductBuilder[T2,TF,TL,Shape2,Shape1Col,Shape1Col],
       ta: TensorArith[(L,F),T2,Tensor2[L,F],Shape2],
       tla: Tensor1Arith[L,TL,TL,Shape1Col],
       datasetModel: DatasetModel[L,F,T2,TL,TF]) {

    val linearizer = TensorLinearizer[(L,F),T2]();
    import linearizer._;

    def objective(data: Seq[Example[L,TF]]) = new ObjectiveFunction(data);

    // preliminaries: an objective function
    class ObjectiveFunction(data: Seq[Example[L,TF]]) extends BatchDiffFunction[(L,F),linearizer.ProjectedTensor] {

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

      override def calculate(flatWeights: ProjectedTensor, range: Seq[Int]) = {
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


  def main(args: Array[String]) {
    import scalala.Scalala._
    import scalala.tensor.operators.DenseMatrixOps._
    import scalala.Scalala._
    import scalanlp.data._
    import scalala.tensor.dense._;                                                                                           

    val data = DataMatrix.fromURL(new java.net.URL("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/spam.data"),-1);
    val vectors = data.rows.map(e => e map ((a:Seq[Double]) => new DenseVector(a.toArray)) relabel (_.toInt))

    val classifier = LogisticClassifier[Int,Int,DenseMatrix,DenseVector,DenseVector](vectors);
    for( ex <- vectors) {
      val guessed = classifier.classify(ex.features);
      println(guessed,ex.label);
    }
  }
  
}
