package scalanlp.classify;

import scalala.tensor.Tensor2Linearizer;
import scalala.tensor._;
import scalala.tensor.operators.TensorShapes._;
import scalala.tensor.operators._;

import scalanlp.data._;
import scalanlp.math.Numerics._;
import scalanlp.optimize._;

object LogisticClassifier {
  def apply[L,F,T2<:Tensor2[L,F] with TensorSelfOp[(L,F),T2,Shape2],
      TL<:Tensor1[L] with TensorSelfOp[L,TL,Shape1Col],
      TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]](data: Seq[Example[L,TF]])
      (implicit tpb: TensorProductBuilder[T2,TF,TL,Shape2,Shape1Col,Shape1Col],
        ta: TensorArith[(L,F),T2,Tensor2[L,F],Shape2],
        tla: Tensor1Arith[L,TL,TL,Shape1Col],
        datasetModel: DatasetModel[L,F,T2,TL,TF]) = {

    require(data.length > 0);
    val linearizer = new Tensor2Linearizer[L,F,T2,Tensor2[L,F]];
    import linearizer._;

    val basisLabel = data.head.label;
    val labelSet = Set.empty ++ data.iterator.map(_.label);
    val featureSet = Set.empty ++ ( 
      for( d <- data.iterator;
        f <- d.features.activeDomain.iterator)
      yield f
    );
    val allLabels = labelSet.toSeq;

    // preliminaries: an objective function
    class ObjectiveFunction(data: Seq[Example[L,TF]]) extends BatchDiffFunction[(L,F),ProjectedTensor] {
      def gradientAt(x: ProjectedTensor, range: Seq[Int]) = calculate(x,range)._2;
      def valueAt(x: ProjectedTensor, range: Seq[Int]) = calculate(x,range)._1;


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

        for( datum <- range map data) {
          val logScores = this.logScores(weights,datum.features);
          val logNormalizer = logSum(logScores.valuesIterator.toSeq);
          ll -= (logScores(datum.label) - logNormalizer);
          assert(!ll.isNaN);
        
          // d ll/d weight_kj = \sum_i x_ij ( I(group_i = k) - p_k(x_i;Beta)) 
          for { 
            label <- allLabels
            if label != basisLabel
            prob_k = Math.exp(logScores(label) - logNormalizer)
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


    val guess = datasetModel.emptyParameterMatrix(data);

    val objective = new ObjectiveFunction(data);

    val opt = new LBFGS[(L,F),ProjectedTensor](-1,5) with scalanlp.util.ConsoleLogging;
    //val opt = new StochasticGradientDescent[(L,F),ProjectedTensor](0.05,0,100000) with scalanlp.util.ConsoleLogging;
    val flatWeights = opt.minimize(objective,linearize(guess));
    val weights = reshape(flatWeights);
    new LinearClassifier[L,F,T2,TL,TF](weights,datasetModel.emptyLabelVector(data));
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
