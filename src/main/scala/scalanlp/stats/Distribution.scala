package scalanlp.stats;

/**
 * Represents a probability Distribution.
 * Different semantics for continuous and discrete distributions.
 * Justification: it enables a uniform definition of ConjugatePriors
 * 
 * @author(dlwh)
 */
trait Distribution[T] extends Rand[T] {
  /**
   * For discrete distributions, return the probability of the element; 
   * for continuous distributions, the pdf evaluated at that element;
   */
  def probabilityOf(t : T) :Double;

  /**
   * For discrete distributions, return the log probability of the element; 
   * for continuous distributions, the log pdf evaluated at that element
   */
  def logProbabilityOf(t : T) : Double = Math.log(probabilityOf(t));

  /**
   * For discrete distributions, return the (possibly unnormalized) prob of the element;
   * for continuous distributions, the pdf up to a constant evaluated at that element
   * For discrete distributions, The following invariant holds:
   * <pre> probabilityOf(t) = unnormalizedProbabilityOf(t) / \sum_t unnormalizedProbabilityOf(t) </pre>
   */
  def unnormalizedProbabilityOf(t :T) : Double = probabilityOf(t);

  /**
   * For discrete distributions, return the (possibly unnormalized) log prob of the element;
   * for continuous distributions, the log pdf up to a constant evaluated at that element
   */
  def unnormalizedLogProbabilityOf(t :T) : Double = logProbabilityOf(t);
}
