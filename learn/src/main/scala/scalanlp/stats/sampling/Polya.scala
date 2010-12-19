package scalanlp.stats.sampling
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

import scalanlp.math.Numerics._;
import scalala.tensor._;
import scalala.operators.{BinaryOp, OpAdd}
;

/**
 * Represents a Polya distribution, a.k.a Dirichlet compound Multinomial distribution
 * see 
 * http://en.wikipedia.org/wiki/Multivariate_Polya_distribution
 *
 * @author dlwh
 */
class Polya[T](prior: Counter[T,Double])(implicit rand: RandBasis=Rand) extends DiscreteDistr[T] {
  private val innerDirichlet = new Dirichlet(prior)(rand);
  def draw() = {
    Multinomial(innerDirichlet.draw)(rand).get;
  }
  
  lazy val logNormalizer = -lbeta(prior);
  
  def probabilityOf(x: T):Double = {
    val ctr = Counter[T,Double]();
    ctr(x) += 1;
    probabilityOf(ctr);
  }
  
  def probabilityOf(x: Counter[T,Double]) = math.exp(logProbabilityOf(x));
  def logProbabilityOf(x: Counter[T,Double]) = {
    math.exp(unnormalizedLogProbabilityOf(x) + logNormalizer);
  }
  
  def unnormalizedLogProbabilityOf(x: Counter[T,Double]):Double = {
    val adjustForCount = x.valuesIterator.foldLeft(lgamma(x.sum+1))( (acc,v) => acc-lgamma(v+1))
    adjustForCount + lbeta(x + prior);
  }

}


object Polya {
  /**
  * Creates a new symmetric Polya of dimension k
  */
  def sym(alpha : Double, k : Int) = this(Array.tabulate(k){ x => alpha });
  
  /**
  * Creates a new Polya of dimension k with the given parameters
  */
  def apply(arr: Array[Double]):Polya[Int] = {
    val swapped: Array[(Int, Double)] = arr.zipWithIndex.map(_.swap)
    new Polya[Int](Counter(swapped.toIndexedSeq:_*))
  }
}
