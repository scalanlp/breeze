package scalanlp.stats.distributions

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

import scalanlp.optimize.DiffFunction
import scalala.generic.collection.{CanViewAsTensor1, CanCreateZerosLike}
import scalala.operators._
import scalala.tensor.mutable.{Tensor1, Tensor}
import scalala.library.Library
import scalala.generic.math.{CanNorm, CanSoftmax}

/**
 * Represents a multinomial Distribution over elements.
 *
 * @author dlwh
 */
case class Multinomial[T,@specialized(Int) I](params: T)(implicit ev: CanViewAsTensor1[T,I,Double], rand: RandBasis=Rand) extends DiscreteDistr[I] {
  val sum = ev(params).sum
  require(sum != 0.0, "There's no mass!")

  // check rep
  for ((k,v) <- ev(params).pairsIterator) {
    if (v < 0) {
      throw new IllegalArgumentException("Multinomial has negative mass at index "+k)
    }
  }

  def draw():I = {
    var prob = rand.uniform.get() * sum
    assert(!prob.isNaN, "NaN Probability!")
    for((i,w) <- ev(params).pairsIteratorNonZero) {
      prob -= w
      if(prob <= 0) return i
    }
    ev(params).keysIteratorNonZero.next
  }

  def probabilityOf(e : I) = ev(params)(e) / sum
  override def unnormalizedProbabilityOf(e:I) = ev(params)(e)/sum

  override def toString = ev(params).pairsIterator.mkString("Multinomial{",",","}")


}


/**
 * Provides routines to create Multinomials
 * @author(dlwh)
 */
object Multinomial {

  class ExpFam[T,I](exemplar: T)
                   (implicit ev: T<:<NumericOps[T] with HasValuesMonadic[T,Double],
                    space: scalala.operators.bundles.MutableInnerProductSpace[Double,T],
                    norm: CanNorm[T],
                    view2: T=>Tensor1[I,Double],
                    view: CanViewAsTensor1[T,I,Double],
                    zeros: CanCreateZerosLike[T,T],
                    softmax: CanSoftmax[T]) extends ExponentialFamily[Multinomial[T,I],I] with HasConjugatePrior[Multinomial[T,I],I] {

    import space._
    type ConjugatePrior = Dirichlet[T,I]
    val conjugateFamily = new Dirichlet.ExpFam[T,I](exemplar)



    def predictive(parameter: conjugateFamily.Parameter) = new Polya(parameter)

    def posterior(prior: conjugateFamily.Parameter, evidence: TraversableOnce[I]) = {
      val copy : T = space.addVS.apply(prior,0.0)
      for( e <- evidence) {
        copy(e)  += 1.0
      }
      copy

    }

    type Parameter = T
    case class SufficientStatistic(t: T) extends scalanlp.stats.distributions.SufficientStatistic[SufficientStatistic] {
      def +(tt: SufficientStatistic) = SufficientStatistic(ev(t) + tt.t)
      def *(w: Double) = SufficientStatistic(ev(t) * w)
    }

    def emptySufficientStatistic = SufficientStatistic(zeros(exemplar))

    def sufficientStatisticFor(t: I) = {
      val r = zeros(exemplar)
      r(t) = 1.0
      SufficientStatistic(r)
    }

    def mle(stats: SufficientStatistic) = ev(stats.t).values.map(math.log _)

    def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[T] {
      def calculate(x: T) = {
        val nn: T = Library.logNormalize(x)
        val lp = nn :* stats.t sum

        val sum = stats.t.sum

        val exped = ev(nn).values.map(math.exp _)
        val grad = ev(ev(exped) * sum) - stats.t

        (-lp,grad)
      }
    }

    def distribution(p: Parameter) = {
      new Multinomial(ev(p).values.map(math.exp _))
    }
  }

}
