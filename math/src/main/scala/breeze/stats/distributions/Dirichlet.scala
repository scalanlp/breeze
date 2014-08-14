package breeze.stats.distributions

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.optimize._
import breeze.linalg._
import breeze.numerics._
import breeze.linalg.operators.{OpDiv, BinaryOp}
import breeze.math._
import breeze.numerics
import breeze.storage.Zero

/**
 * Represents a Dirichlet distribution, the conjugate prior to the multinomial.
 * @author dlwh
 */
case class Dirichlet[T, @specialized(Int) I](params: T)(implicit space: EnumeratedCoordinateField[T, I, Double],
                                                        rand: RandBasis = Rand) extends ContinuousDistr[T] {
  import space._
  /**
   * Returns a Multinomial distribution over the iterator
   */
  def draw():T = {
    normalize(unnormalizedDraw(),1.0)
  }

  /**
   * Returns unnormalized probabilities for a Multinomial distribution.
   */
  def unnormalizedDraw() = {
    mapValues.mapActive(params, { (v:Double) => new Gamma(v,1).draw()})
  }

  /**
   * Returns logNormalized probabilities. Use this if you're worried about underflow
   */
  def logDraw() = {
    val x = mapValues.mapActive(params, { (v:Double) => new Gamma(v,1).logDraw()})
    val m = softmax(x.activeValuesIterator)
    assert(!m.isInfinite, x)
    x.activeKeysIterator.foreach(i => x(i) -= m)
    x
  }

  /**
   * Returns the log pdf function of the Dirichlet up to a constant evaluated at m
   */
  override def unnormalizedLogPdf(m : T) = {
    val parts = for( (k,v) <- params.activeIterator) yield (v-1) * math.log(m(k))
    parts.sum
  }

  lazy val logNormalizer = lbeta(params)

  /**
   * Returns a Polya Distribution
   */
//  def predictive() = new Polya(params)(rand)

}

/**
 * Provides several defaults for Dirichlets, one for Arrays and one for
 * Counters.
 *
 * @author dlwh
 */
object Dirichlet {
  /**
   * Creates a new Dirichlet with pseudocounts equal to the observed counts.
   */
  def apply[T](c : Counter[T,Double]) = new Dirichlet(c)

  /**
   * Creates a new symmetric Dirichlet of dimension k
   */
  def sym(alpha : Double, k : Int) = this(Array.tabulate(k){ x => alpha })

  def apply(arr: Array[Double]): Dirichlet[DenseVector[Double], Int] = Dirichlet( new DenseVector[Double](arr))


  class ExpFam[T,I](exemplar: T)(implicit space: MutableFiniteCoordinateField[T, I, Double]) extends ExponentialFamily[Dirichlet[T,I],T] {
    import space._
    type Parameter = T
    case class SufficientStatistic(n: Double, t: T) extends breeze.stats.distributions.SufficientStatistic[SufficientStatistic] {
      // TODO: use online mean here
      def +(tt: SufficientStatistic) = SufficientStatistic(n + tt.n, t + tt.t)
      def *(w: Double) = SufficientStatistic(n * w, t * w)
    }

    def emptySufficientStatistic = SufficientStatistic(0,zeroLike(exemplar))

    def sufficientStatisticFor(t: T) = {
      SufficientStatistic(1,numerics.log(normalize(t,1.0)))
    }

    def mle(stats: SufficientStatistic) = {
      val likelihood = likelihoodFunction(stats)
      val result = minimize(likelihood, zeroLike(stats.t) :+ 1.0)
      result
    }

    def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[T] {
      val p = stats.t / stats.n
      def calculate(x: T) = {
        val lp = -stats.n * (-lbeta(x)  + ((x - 1.0) dot p))
        val grad: T = (digamma(x) - digamma(sum(x))  - p) * (stats.n)
        if(lp.isNaN) (Double.PositiveInfinity,grad)
        else (lp,grad)
      }
    }

    def distribution(p: Parameter) = {
      new Dirichlet(p)
    }
  }
}
