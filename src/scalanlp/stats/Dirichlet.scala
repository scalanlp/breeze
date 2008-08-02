package scalanlp.stats;
import scalanlp.counters._;
import scalanlp.counters.DoubleCounter;
import scalanlp.counters.Counters._;
import scalanlp.math.Numerics._;

/**
 * Represents a Dirichlet distribution, the conjugate prior to the multinomial.
 * @author dlwh
 */
trait Dirichlet[T] extends ConjugatePrior[Multinomial[T],T] {
  protected val prior : Iterable[(T,Double)];
  private lazy val generators : Iterable[(T,Gamma)] = prior.map { e => (e._1,new Gamma(e._2,1)) }

  /**
   * Provides access to the components of the Dirichlet, for inspection.
   */
  def elements = prior.elements;

  /**
   * Returns a Multinomial distribution over the elements;
   */
  def get() = {
    Multinomial(aggregate(generators.map(e => (e._1,e._2.get)).elements));
  }

  /**
   * Returns the log pdf function of the Dirichlet up to a constant evaluated at m
   */
  override def unnormalizedLogProbabilityOf(m : Multinomial[T]) = {
    prior.map( e => (e._2-1) * m.logProbabilityOf(e._1)).
          foldLeft(0.0)(_+_);
  }

  /**
   * Returns the log pdf of the Dirichlet evaluated at m.
   */
  override def logProbabilityOf(m: Multinomial[T]) = {
    unnormalizedLogProbabilityOf(m) + prior.map(e => lgamma(e._2))
      .foldLeft(0.0)(_+_) - lgamma(prior.foldLeft(0.0)(_+_._2));
  }

  /**
   * Returns the pdf of the Dirichlet evaluated at m.
   */
  def probabilityOf(m : Multinomial[T]) = Math.exp(logProbabilityOf(m));

  /**
   * Returns a multinomial over T's. Each draw is drawn from E[Dir]
   */
  def predictive : Multinomial[T] = Multinomial(aggregate(prior.elements).normalized);

  /**
   * Returns a new Dirichlet after observing the evidence. 
   */
  override def posterior(evidence : Iterator[(T,Double)]) = Dirichlet(aggregate(prior.elements ++ evidence));
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
  def apply[T](c : DoubleCounter[T]) = new Dirichlet[T] {
    protected val prior = c.elements.toList; 
  }

  /**
   * Creates a new symmetric Dirichlet of dimension k
   */
  def sym(alpha : Double, k : Int) = this(Array.fromFunction{ x => alpha }(k));

  /**
   * Creates a new Dirichlet with pseudocounts arr(i)
   */
  def apply(arr : Array[Double]) = new Dirichlet[Int]{
    val prior = arr.zipWithIndex.map{x => (x._2,x._1)};
    // a little faster
    override def get() = {
      val arr = generators.map(e => e._2.get)
      Multinomial(arr.toSeq.toArray,arr.foldLeft(0.0)(_+_));
    }
  }
}
