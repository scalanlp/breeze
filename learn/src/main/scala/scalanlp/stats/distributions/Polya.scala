package scalanlp.stats.distributions
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

import scalala.library.Numerics._;
import scalala.tensor._;
import scalala.generic.math.CanNorm
import scalala.generic.collection.{CanMapValues, CanViewAsTensor1}
import scalala.operators._
;

/**
 * Represents a Polya distribution, a.k.a Dirichlet compound Multinomial distribution
 * see 
 * http://en.wikipedia.org/wiki/Multivariate_Polya_distribution
 *
 * @author dlwh
 */
class Polya[T,@specialized(Int) I](params: T)(implicit ev: CanViewAsTensor1[T,I,Double],
                                              space: scalala.operators.bundles.MutableInnerProductSpace[Double,T],
                                              numeric: T<:<NumericOps[T],
                                              monadic: T=>HasValuesMonadic[T,Double],
                                              norm: CanNorm[T],
                                              rand: RandBasis=Rand) extends DiscreteDistr[I] {
  import space._;
  private val innerDirichlet = new Dirichlet(params);
  def draw() = {
    Multinomial(innerDirichlet.draw).get;
  }

  lazy val logNormalizer = -lbeta(ev(params));

  def probabilityOf(x: I) = math.exp(lbeta2(ev(params).sum,1) - lbeta2(ev(params)(x),1))

  private def lbeta2(a: Double, b: Double) = lgamma(a) + lgamma(b) - lgamma(a+b)

//  def probabilityOf(x: T) = math.exp(logProbabilityOf(x));
//  def logProbabilityOf(x: T) = {
//    math.exp(unnormalizedLogProbabilityOf(x) + logNormalizer);
//  }
//
//  def unnormalizedLogProbabilityOf(x: T):Double = {
//    val adjustForCount = ev(x).valuesIterator.foldLeft(lgamma(ev(x).sum+1))( (acc,v) => acc-lgamma(v+1))
//    adjustForCount + lbeta(ev(x + params));
//  }

}


object Polya {
  /**
  * Creates a new symmetric Polya of dimension k
  */
  def sym(alpha : Double, k : Int) = this(Array.tabulate(k){ x => alpha });
  
  /**
  * Creates a new Polya of dimension k with the given parameters
  */
  def apply(arr: Array[Double]) = {
    val swapped: Array[(Int, Double)] = arr.zipWithIndex.map(_.swap)
    new Polya(Counter(swapped.toIndexedSeq:_*))
  }
}
