package scalanlp.stats;

/**
 * Trait representing conjugate priors. See Dirichlet for an example.
 */
trait ConjugatePrior[D,T] extends Distribution[D] {
  /**
   * Returns a distribtution over T's after integrating out the intermediate distributions.
   */
  def predictive() : Distribution[T];
  /**
   * Gives a new ConjugatePrior after observing the evidence. See Dirichlet for an example.
   */
  def posterior(evidence : Iterable[(T,Double)]) : ConjugatePrior[D,T] = posterior(evidence.elements);
  /**
   * Gives a new ConjugatePrior after observing the evidence. See Dirichlet for an example.
   */
  def posterior(evidence : Iterator[(T,Double)]) : ConjugatePrior[D,T];
}
