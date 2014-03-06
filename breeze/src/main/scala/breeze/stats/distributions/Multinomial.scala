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
import breeze.math.{MutablizingAdaptor, VectorSpace, TensorSpace, MutableCoordinateSpace}
import breeze.numerics._
import breeze.numerics
import breeze.storage.DefaultArrayValue

/**
 * Represents a Multinomial distribution over elements.
 * You can make a distribution over any [[breeze.linalg.QuasiTensor]], which includes
 * DenseVectors and Counters.
 *
 * TODO: I should probably rename this to Discrete or something, since it only handles
 * one draw.
 *
 * @author dlwh
 */
case class Multinomial[T,I](params: T)(implicit ev: T=>QuasiTensor[I, Double], sumImpl: breeze.linalg.sum.Impl[T, Double], rand: RandBasis=Rand) extends DiscreteDistr[I] {
  val sum = breeze.linalg.sum(params)
  require(sum != 0.0, "There's no mass!")

  // check rep
  for ((k,v) <- params.activeIterator) {
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

  def expectedValue[U](f: I=>U)(implicit vs: VectorSpace[U, Double]) = {
    val wrapped = MutablizingAdaptor.ensureMutable(vs)
    import wrapped.mutaVspace._
    import wrapped._
    var acc: Wrapper = null.asInstanceOf[Wrapper]
    for ( (k, v) <- params.activeIterator) {
      if(acc == null) {
        acc = wrap(f(k)) * (v/sum)
      } else {
        axpy(v/sum, wrap(f(k)), acc)
      }
    }
    assert(acc != null)
    unwrap(acc)
  }


}


/**
 * Provides routines to create Multinomials
 * @author dlwh
 */
object Multinomial {

  class ExpFam[T,I](exemplar: T)(implicit space: TensorSpace[T, I, Double], dav: DefaultArrayValue[T]) extends ExponentialFamily[Multinomial[T,I],I] with HasConjugatePrior[Multinomial[T,I],I] {

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
    case class SufficientStatistic(counts: T) extends breeze.stats.distributions.SufficientStatistic[SufficientStatistic] {
      def +(tt: SufficientStatistic) = SufficientStatistic(counts + tt.counts)
      def *(w: Double) = SufficientStatistic(counts * w)
    }

    def emptySufficientStatistic = SufficientStatistic(zeros(exemplar))

    def sufficientStatisticFor(t: I) = {
      val r = zeros(exemplar)
      r(t) = 1.0
      SufficientStatistic(r)
    }

    def mle(stats: SufficientStatistic) = log(stats.counts)

    def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[T] {
      def calculate(x: T) = {
        val nn: T = logNormalize(x)
        val lp = nn dot stats.counts

        val mysum = sum(stats.counts)


        val exped = numerics.exp(nn)
        val grad = exped * mysum - stats.counts

        (-lp,grad)
      }
    }

    def distribution(p: Parameter) = {
      new Multinomial(numerics.exp(p))
    }
  }

}
