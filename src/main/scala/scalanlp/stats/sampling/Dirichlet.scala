package scalanlp.stats.sampling;

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

import scalanlp.counters._;
import scalanlp.collection.mutable.ArrayMap;
import scala.collection.mutable.ArrayBuffer;
import scalanlp.counters.Counters._;
import scalanlp.math.Numerics._;

/**
 * Represents a Dirichlet distribution, the conjugate prior to the multinomial.
 * @author dlwh
 */
class Dirichlet[T](prior: DoubleCounter[T]) extends ContinuousDistr[DoubleCounter[T]] with ConjugatePrior[DoubleCounter[T],T] {
  /**
   * Returns a new Dirichlet after observing the evidence. 
   */
  override def posterior(evidence : Iterator[(T,Int)]): Dirichlet[T] = {
    val ctr = aggregate(evidence.map(x => (x._1,x._2.toDouble)));
    ctr += prior;
    new Dirichlet(prior);
  }

  private val generators : Iterable[(T,Gamma)] = prior.iterator.map { (e:(T,Double)) => (e._1,new Gamma(e._2,1)) } toSeq;

  /**
   * Provides access to the components of the Dirichlet, for inspection.
   */
  def components = prior.iterator;

  /**
   * Returns a Multinomial distribution over the iterator;
   */
  def draw() = {
    aggregate(generators.map(e => (e._1,e._2.draw)).iterator);
  }

  /**
   * Returns unnormalized probabilities for a Multinomial distribution.
   */
  def unnormalizedDraw() = {
    aggregate(generators.map(e => (e._1,e._2.draw)).iterator)
  }

  /**
   * Returns the log pdf function of the Dirichlet up to a constant evaluated at m
   */
  override def unnormalizedLogPdf(m : DoubleCounter[T]) = {
    prior.iterator.map( e => (e._2-1) * m(e._1) ).foldLeft(0.0)(_+_);
  }

  val logNormalizer = lgamma(prior.foldLeft(0.0)(_+_._2))  - prior.map( (e:(T,Double)) => lgamma(e._2)).foldLeft(0.0)(_+_) 

  /**
   * Returns a Polya Distribution
   */
  def predictive = new Polya(aggregate(prior.iterator));

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
  def apply[T](c : DoubleCounter[T]): Dirichlet[T] = new Dirichlet[T](c);

  /**
   * Creates a new symmetric Dirichlet of dimension k
   */
  def sym(alpha : Double, k : Int) = this(Array.tabulate(k){ x => alpha });
  
  def apply(arr: Array[Double]):Dirichlet[Int] = Dirichlet( aggregate(arr.zipWithIndex.map(_.swap)));
}
