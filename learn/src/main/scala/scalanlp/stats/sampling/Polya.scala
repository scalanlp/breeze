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

import scala.collection.mutable.ArrayBuffer;

import scalanlp.math.Numerics._;
import scalala.tensor.counters._;
import Counters._;
import scalanlp.collection.mutable.ArrayMap;

/**
 * Represents a Polya distribution, a.k.a Dirichlet compound Multinomial distribution
 * see 
 * http://en.wikipedia.org/wiki/Multivariate_Polya_distribution
 *
 * @author dlwh
 */
class Polya[T](prior: DoubleCounter[T])(implicit rand: RandBasis=Rand) extends DiscreteDistr[T] {
  private val innerDirichlet = new Dirichlet(prior)(rand);
  def draw() = {
    Multinomial(innerDirichlet.draw)(rand).get;
  }
  
  val logNormalizer = -lbeta(prior);
  
  def probabilityOf(x: T):Double = {
    val ctr = DoubleCounter[T]();
    ctr.incrementCount(x,1);
    probabilityOf(ctr);
  }
  
  def probabilityOf(x: DoubleCounter[T]) = math.exp(logProbabilityOf(x));
  def logProbabilityOf(x: DoubleCounter[T]) = {
    math.exp(unnormalizedLogProbabilityOf(x) + logNormalizer);
  }
  
  def unnormalizedLogProbabilityOf(x: DoubleCounter[T]):Double = {
    val ctr = aggregate(x.map( (x: (T,Double)) => (x._1, x._2)));
    val adjustForCount = x.valuesIterator.foldLeft(lgamma(x.total+1))( (acc,v) => acc-lgamma(v+1))
    ctr += prior;
    adjustForCount + lbeta(ctr);
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
  def apply(arr: Array[Double]):Polya[Int] = new Polya[Int]( Counters.aggregate(arr.zipWithIndex.map(_.swap):_*))
}
