package scalanlp.stats.sampling

import scala.collection.mutable.ArrayBuffer;

import scalanlp.math.Numerics._;
import scalala.tensor.counters._;
import Counters._;
import scalanlp.collection.mutable.ArrayMap;

/**
 * Represents a Polya distribution, a.k.a Dirichlet compound Multinomial distribution
 * see 
 * http://en.wikipedia.org/wiki/Multivariate_Polya_distribution
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
