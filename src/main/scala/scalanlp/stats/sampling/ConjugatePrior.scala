package scalanlp.stats.sampling;

/**
 * Trait representing conjugate priors. See Dirichlet for an example.
 */
trait ConjugatePrior[P,T] extends Measure[P] {
  /**
   * Returns a distribtution over T's after integrating out the intermediate distributions.
   */
  def predictive() : Measure[T];
  /**
   * Gives a new ConjugatePrior after observing the evidence. See Dirichlet for an example.
   */
  def posterior(evidence : Iterable[(T,Int)]) : ConjugatePrior[P,T] = posterior(evidence.elements);
  /**
   * Gives a new ConjugatePrior after observing the evidence. See Dirichlet for an example.
   */
  def posterior(evidence : Iterator[(T,Int)]) : ConjugatePrior[P,T];
}
