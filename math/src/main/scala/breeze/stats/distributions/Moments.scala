package breeze.stats.distributions

/**
 * Interface for distributions that can report on some of their moments
 *
 * @author dlwh
 **/
trait Moments[Mean, Variance] {
  def mean: Mean
  def variance: Variance
  def entropy: Double
  def mode: Mean
}
