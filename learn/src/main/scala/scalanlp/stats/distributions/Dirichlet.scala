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
import scalala.library.Numerics._;

/**
 * Represents a Dirichlet distribution, the conjugate prior to the multinomial.
 * @author dlwh
 */
class Dirichlet[T](prior: Counter[T,Double])(implicit rand: RandBasis = Rand) extends ContinuousDistr[Counter[T,Double]] with ConjugatePrior[Counter[T,Double],T] {
  /**
   * Returns a new Dirichlet after observing the evidence. 
   */
  override def posterior(evidence : Iterator[(T,Int)]): Dirichlet[T] = {
    val ctr = Counter(evidence.map(x => (x._1,x._2.toDouble)).toSeq:_*);
    ctr += prior;
    new Dirichlet(prior)(rand);
  }

  private val generators : Seq[(T,Gamma)] = prior.pairs.iterator.map { (e:(T,Double)) => (e._1,new Gamma(e._2,1)(rand)) } toSeq;

  /**
   * Provides access to the components of the Dirichlet, for inspection.
   */
  def components = prior.pairs.iterator;

  /**
   * Returns a Multinomial distribution over the iterator;
   */
  def draw() = {
    Counter(generators.map(e => (e._1,e._2.draw)):_*);
  }

  /**
   * Returns unnormalized probabilities for a Multinomial distribution.
   */
  def unnormalizedDraw() = {
    Counter(generators.map(e => (e._1,e._2.draw)):_*)
  }

  /**
   * Returns the log pdf function of the Dirichlet up to a constant evaluated at m
   */
  override def unnormalizedLogPdf(m : Counter[T,Double]) = {
    prior.pairs.iterator.map( e => (e._2-1) * m(e._1) ).foldLeft(0.0)(_+_);
  }

  val logNormalizer = lgamma(prior.sum)  - prior.valuesIterator.map( e => lgamma(e)).sum;

  /**
   * Returns a Polya Distribution
   */
  def predictive() = new Polya(prior)(rand);

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
  def apply[T](c : Counter[T,Double]): Dirichlet[T] = new Dirichlet[T](c);

  /**
   * Creates a new symmetric Dirichlet of dimension k
   */
  def sym(alpha : Double, k : Int) = this(Array.tabulate(k){ x => alpha });
  
  def apply(arr: Array[Double]):Dirichlet[Int] = Dirichlet( Counter(arr.zipWithIndex.map(_.swap):_*));
}
