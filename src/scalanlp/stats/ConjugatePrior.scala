package scalanlp.stats;

/**
 * Trait representing conjugate priors. See Dirichlet for an example.
 */
trait ConjugatePrior[P,T] extends Distribution[P] {
  /**
   * Returns a distribtution over T's after integrating out the intermediate distributions.
   */
  def predictive() : Distribution[T];
  /**
   * Gives a new ConjugatePrior after observing the evidence. See Dirichlet for an example.
   */
  def posterior(evidence : Iterable[(T,Double)]) : ConjugatePrior[P,T] = posterior(evidence.elements);
  /**
   * Gives a new ConjugatePrior after observing the evidence. See Dirichlet for an example.
   */
  def posterior(evidence : Iterator[(T,Double)]) : ConjugatePrior[P,T];
}
