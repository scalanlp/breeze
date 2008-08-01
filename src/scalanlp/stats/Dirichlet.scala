package scalanlp.stats;
import scalanlp.counters._;
import scalanlp.counters.DoubleCounter;
import scalanlp.counters.Counters._;
import scalanlp.math.Numerics._;

trait Dirichlet[T] extends ConjugatePrior[Multinomial[T],T] {
  protected val prior : Iterable[(T,Double)];
  private lazy val generators : Iterable[(T,Gamma)] = prior.map { e => (e._1,new Gamma(e._2,1)) }

  def elements = prior.elements;

  def get() = {
    Multinomial(aggregate(generators.map(e => (e._1,e._2.get)).elements));
  }
  override def unnormalizedLogProbabilityOf(m : Multinomial[T]) = {
    prior.map( e => (e._2-1) * m.logProbabilityOf(e._1)).
          foldLeft(0.0)(_+_);
  }
  override def logProbabilityOf(m: Multinomial[T]) = {
    unnormalizedLogProbabilityOf(m) + prior.map(e => lgamma(e._2))
      .foldLeft(0.0)(_+_) - lgamma(prior.foldLeft(0.0)(_+_._2));
  }
  def probabilityOf(m : Multinomial[T]) = Math.exp(logProbabilityOf(m));

  def predictive : Multinomial[T] = Multinomial(aggregate(prior.elements).normalized);

  override def posterior(evidence : Iterator[(T,Double)]) = Dirichlet(aggregate(prior.elements ++ evidence));

}

object Dirichlet {
  def apply[T](c : DoubleCounter[T]) = new Dirichlet[T] {
    protected val prior = c.elements.toList; 
  }

  def sym(alpha : Double, k : Int) = this(aggregate( (0 until k map {x :Int => (x,alpha)}).elements));

  def apply(arr : Array[Double]) = new Dirichlet[Int]{
    val prior = arr.zipWithIndex.map{x => (x._2,x._1)};
    // a little faster
    override def get() = {
      val arr = generators.map(e => e._2.get)
      Multinomial(arr.toList.toArray,arr.foldLeft(0.0)(_+_));
    }
  }
}
