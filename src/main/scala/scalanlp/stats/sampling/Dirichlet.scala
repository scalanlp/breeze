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
import scalanlp.counters.DoubleCounter;
import scalanlp.counters.Counters._;
import scalanlp.math.Numerics._;
import scalanlp.math.Arrays._;

/**
 * Represents a Dirichlet distribution, the conjugate prior to the multinomial.
 * @author dlwh
 */
trait Dirichlet[P,T] extends ContinuousDistr[P] with ConjugatePrior[P,T] {
  protected val prior : Iterable[(T,Double)];
  protected def pFromDraw(it : Iterator[(T,Double)]) : P;
  protected def componentsFromP(p : P) : Function[T,Double]
  /**
   * Returns a new Dirichlet after observing the evidence. 
   */
  override def posterior(evidence : Iterator[(T,Int)]): Dirichlet[P,T];


  private val generators : Iterable[(T,Gamma)] = prior.map { e => (e._1,new Gamma(e._2,1)) }

  /**
   * Provides access to the components of the Dirichlet, for inspection.
   */
  def components = prior.elements;

  /**
   * Returns a Multinomial distribution over the elements;
   */
  def draw() = {
    pFromDraw(generators.map(e => (e._1,e._2.get)).elements);
  }

  /**
   * Returns the log pdf function of the Dirichlet up to a constant evaluated at m
   */
  override def unnormalizedLogPdf(m : P) = {
    val f = componentsFromP(m);
    prior.map( e => (e._2-1) * f(e._1) ).
          foldLeft(0.0)(_+_);
  }

  /**
   * Returns the log pdf of the Dirichlet evaluated at m.
   */
  override def logPdf(m: P) = {
    unnormalizedLogPdf(m) + logNormalizer;
  }

  val logNormalizer = prior.map(e => lgamma(e._2)).foldLeft(0.0)(_+_) - lgamma(prior.foldLeft(0.0)(_+_._2));

  /**
   * Returns the pdf of the Dirichlet evaluated at m.
   */
  def pdf(m : P) = Math.exp(logPdf(m));

  /**
   * Returns a multinomial over T's. Each draw is drawn from E[Dir]
   */
  def predictive : Multinomial[T] = Multinomial(aggregate(prior.elements).normalized);

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
  def apply[T](c : DoubleCounter[T]): Dirichlet[DoubleCounter[T],T] = new Dirichlet[DoubleCounter[T],T] {
    protected val prior = c.elements.toList; 
    protected def pFromDraw(it : Iterator[(T,Double)]) = aggregate(it);
    protected def componentsFromP(p : DoubleCounter[T]) = p;
    def posterior(it : Iterator[(T,Int)]) : Dirichlet[DoubleCounter[T],T] = {
      Dirichlet( (DoubleCounter[T]() ++ it.map{ case (a,b) => (a,b.toDouble)} ++ prior).asInstanceOf[DoubleCounter[T]]);
    }
  }

  /**
   * Creates a new symmetric Dirichlet of dimension k
   */
  def sym(alpha : Double, k : Int) = this(Array.fromFunction{ x => alpha }(k));

  /**
   * Creates a new Dirichlet with pseudocounts arr(i)
   */
  def apply(arr : Array[Double]) : Dirichlet[Array[Double],Int] = new Dirichlet[Array[Double], Int] {
    val prior = arr.zipWithIndex.map{x => (x._2,x._1)};
    def pFromDraw(it : Iterator[(Int,Double)]) = {
      val m = Map() ++ it;
      Array.fromFunction(m)(prior.length);
    }

    protected def componentsFromP(p : Array[Double]) = {
      p;
    }

    def posterior(it : Iterator[(Int,Int)]) = { 
      val ret = new Array[Double](prior.length);
      System.arraycopy(arr,0,ret,0,ret.length);
      for( (t,d) <- it) {
        ret(t) += d;
      }
      Dirichlet(ret);
    }

    // a little faster
    override def get() = {
      val arr = generators.map(e => e._2.get)
      normalize(arr.toSeq.toArray)
    }
  }
}
