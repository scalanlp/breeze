package scalanlp.stats.distributions;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
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
import dense.DenseVectorCol
import scalala.library.Numerics._
import scalala.library.Library
import scalala.operators._
import bundles.MutableInnerProductSpace
import scalala.generic.collection.{CanMapValues, CanViewAsTensor1}
import scalala.generic.math.{CanSoftmax, CanNorm}
import scalanlp.optimize.{LBFGS, DiffFunction}

/**
 * Represents a Dirichlet distribution, the conjugate prior to the multinomial.
 * @author dlwh
 */
case class Dirichlet[T,@specialized(Int) I](params: T)(implicit ev: CanViewAsTensor1[T,I,Double],
                                                       numeric: T<:<NumericOps[T],
                                                       monadic: T=>HasValuesMonadic[T,Double],
                                                       norm: CanNorm[T],
                                                       div: BinaryOp[T,Double,OpDiv,T],
                                                       canMapValues: CanMapValues[T,Double,Double,T],
                                                       rand: RandBasis=Rand) extends ContinuousDistr[T] {
  /**
   * Returns a Multinomial distribution over the iterator;
   */
  def draw():T = {
    Library.normalize(unnormalizedDraw(),1)
  }

  /**
   * Returns unnormalized probabilities for a Multinomial distribution.
   */
  def unnormalizedDraw() = {
    params.values.map{ v => new Gamma(v,1).draw()}:T
  }

  /**
   * Returns the log pdf function of the Dirichlet up to a constant evaluated at m
   */
  override def unnormalizedLogPdf(m : T) = {
    val parts = for( (k,v) <- ev(params).pairsIteratorNonZero) yield (v-1) * math.log(ev(m)(k));
    parts.sum
  }

  val logNormalizer = lbeta(ev(params))

  /**
   * Returns a Polya Distribution
   */
//  def predictive() = new Polya(params)(rand);

}

/**
 * Provides several defaults for Dirichlets, one for Arrays and one for
 * Counters.
 *
 * @author(dlwh)
 */
object Dirichlet {
  /**
   * Creates a new Dirichlet with pseudocounts equal to the observed counts.
   */
  def apply[T](c : Counter[T,Double]) = new Dirichlet(c);

  /**
   * Creates a new symmetric Dirichlet of dimension k
   */
  def sym(alpha : Double, k : Int) = this(Array.tabulate(k){ x => alpha });
  
  def apply(arr: Array[Double]): Dirichlet[DenseVectorCol[Double], Int] = Dirichlet( new DenseVectorCol[Double](arr));


  class ExpFam[T,I](exemplar: T)
                   (implicit ev: T<:<NumericOps[T] with HasValuesMonadic[T,Double],
                    space: MutableInnerProductSpace[Double,T], norm: CanNorm[T],
                    view: T=>Tensor1[I,Double],
                    softmax: CanSoftmax[T]) extends ExponentialFamily[Dirichlet[T,I],T] {
    import space._;
    type Parameter = T
    case class SufficientStatistic(n: Double, t: T) extends scalanlp.stats.distributions.SufficientStatistic[SufficientStatistic] {
      // TODO: use online mean here
      def +(tt: SufficientStatistic) = SufficientStatistic(n + tt.n, ev(t) + tt.t);
      def *(w: Double) = SufficientStatistic(n * w, ev(t) * w);
    }

    def emptySufficientStatistic = SufficientStatistic(0,zeros(exemplar));

    def sufficientStatisticFor(t: T) = {
      SufficientStatistic(1,ev(Library.normalize(t,1)).values.map(math.log _))
    }

    def mle(stats: SufficientStatistic) = {
      val likelihood = likelihoodFunction(stats);
      val lbfgs = new LBFGS[T](100,3)
      val result = lbfgs.minimize(likelihood,ev(zeros(stats.t)) + 1.0);
      result;
    }

    def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[T] {
      val p = ev(stats.t) / stats.n
      def calculate(x: T) = {
        val lp = -stats.n * (-lbeta(x)  + (ev(ev(x) - 1.0) dot p))
        val grad: T = ev(ev(ev(ev(x).values.map(digamma))  - digamma(x.sum))  - p) * (stats.n)
        if(lp.isNaN) (Double.PositiveInfinity,grad)
        else (lp,grad)
      }
    }

    def distribution(p: Parameter) = {
      implicit val view2 = new CanViewAsTensor1[T,I,Double] {
        def apply(from: T) = view(from);
      }
      new Dirichlet(p);
    }
  }
}
