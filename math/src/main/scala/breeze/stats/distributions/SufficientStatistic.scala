package breeze.stats.distributions

/**
 *
 * @author dlwh
 */
trait SufficientStatistic[T <: SufficientStatistic[T]] {  self: T =>
  def +(t: T): T
  def *(weight: Double): T
}
