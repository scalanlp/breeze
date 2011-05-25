package scalanlp.stats.distributions

/**
 * 
 * @author dlwh
 */

trait SufficientStatistic[T<:SufficientStatistic[T]] { this: T=>
  def +(t: T):T
  def *(weight: Double):T
}