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

import breeze.optimize.DiffFunction
import breeze.linalg._
import breeze.math.{TensorSpace, MutableCoordinateSpace}
import breeze.numerics._
import breeze.numerics

/**
 * Represents a multinomial Distribution over elements.
 *
 * @author dlwh
 */
case class Multinomial[T,I](params: T)(implicit ev: T=>QuasiTensor[I, Double], rand: RandBasis=Rand) extends DiscreteDistr[I] {
  val sum = params.sum
  require(sum != 0.0, "There's no mass!")

  // check rep
  for ((k,v) <- params.iterator) {
    if (v < 0) {
      throw new IllegalArgumentException("Multinomial has negative mass at index "+k)
    }
  }

  def draw():I = {
    var prob = rand.uniform.get() * sum
    assert(!prob.isNaN, "NaN Probability!")
    for((i,w) <- params.activeIterator) {
      prob -= w
      if(prob <= 0) return i
    }
    params.activeKeysIterator.next()
  }

  def probabilityOf(e : I) = params(e) / sum
  override def unnormalizedProbabilityOf(e:I) = params(e)

  override def toString = ev(params).activeIterator.mkString("Multinomial{",",","}")


}


/**
 * Provides routines to create Multinomials
 * @author(dlwh)
 */
object Multinomial {

  class ExpFam[T,I](exemplar: T)(implicit space: TensorSpace[T, I, Double]) extends ExponentialFamily[Multinomial[T,I],I] with HasConjugatePrior[Multinomial[T,I],I] {

    import space._
    type ConjugatePrior = Dirichlet[T,I]
    val conjugateFamily = new Dirichlet.ExpFam[T,I](exemplar)



    def predictive(parameter: conjugateFamily.Parameter) = new Polya(parameter)

    def posterior(prior: conjugateFamily.Parameter, evidence: TraversableOnce[I]) = {
      val copy : T = space.copy(prior)
      for( e <- evidence) {
        copy(e)  += 1.0
      }
      copy

    }

    type Parameter = T
    case class SufficientStatistic(t: T) extends breeze.stats.distributions.SufficientStatistic[SufficientStatistic] {
      def +(tt: SufficientStatistic) = SufficientStatistic(t + tt.t)
      def *(w: Double) = SufficientStatistic(t * w)
    }

    def emptySufficientStatistic = SufficientStatistic(zeros(exemplar))

    def sufficientStatisticFor(t: I) = {
      val r = zeros(exemplar)
      r(t) = 1.0
      SufficientStatistic(r)
    }

    def mle(stats: SufficientStatistic) = log(stats.t)

    def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[T] {
      def calculate(x: T) = {
        val nn: T = logNormalize(x)
        val lp = nn dot stats.t

        val sum = stats.t.sum

        val exped = numerics.exp(nn)
        val grad = exped * sum - stats.t

        (-lp,grad)
      }
    }

    def distribution(p: Parameter) = {
      new Multinomial(numerics.exp(p))
    }
  }

}
