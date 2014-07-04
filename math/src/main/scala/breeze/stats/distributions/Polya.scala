package breeze.stats.distributions

import breeze.linalg.{sum, Counter, NumericOps}
import breeze.math.{TensorSpace, MutableCoordinateSpace}
import breeze.numerics._
import breeze.storage.Zero


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

/**
 * Represents a Polya distribution, a.k.a Dirichlet compound Multinomial distribution
 * see 
 * http://en.wikipedia.org/wiki/Multivariate_Polya_distribution
 *
 * @author dlwh
 */
class Polya[T,@specialized(Int) I](params: T)(implicit space: TensorSpace[T, I, Double],
                                              rand: RandBasis=Rand) extends DiscreteDistr[I] {
  import space._
  private val innerDirichlet = new Dirichlet(params)
  def draw() = {
    Multinomial(innerDirichlet.draw).get
  }

  lazy val logNormalizer = -lbeta(params)

  def probabilityOf(x: I) = math.exp(lbeta(sum(params), 1.0) - lbeta(params(x), 1.0))


//  def probabilityOf(x: T) = math.exp(logProbabilityOf(x))
//  def logProbabilityOf(x: T) = {
//    math.exp(unnormalizedLogProbabilityOf(x) + logNormalizer)
//  }
//
//  def unnormalizedLogProbabilityOf(x: T):Double = {
//    val adjustForCount = ev(x).valuesIterator.foldLeft(lgamma(ev(x).sum+1))( (acc,v) => acc-lgamma(v+1))
//    adjustForCount + lbeta(ev(x + params))
//  }

}


object Polya {
  /**
  * Creates a new symmetric Polya of dimension k
  */
  def sym(alpha : Double, k : Int) = this(Array.tabulate(k){ x => alpha })

  /**
  * Creates a new Polya of dimension k with the given parameters
  */
  def apply(arr: Array[Double]) = {
    val swapped: Array[(Int, Double)] = arr.zipWithIndex.map(_.swap)
    new Polya(Counter(swapped.toIndexedSeq:_*))
  }
}
