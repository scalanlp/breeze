package scalanlp.stats;
import scalanlp.counters._;
import scalanlp.math.Numerics._;

trait ConjugatePrior[D,T] extends Distribution[D] {
  def predictive : Distribution[T];
  def posterior(evidence : Iterable[(T,Double)]) : ConjugatePrior[D,T] = posterior(evidence.elements);
  def posterior(evidence : Iterator[(T,Double)]) : ConjugatePrior[D,T];
}
