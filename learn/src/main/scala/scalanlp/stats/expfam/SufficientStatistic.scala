package scalanlp.stats.expfam

/**
 * 
 * @author dlwh
 */

trait SufficientStatistic[T<:SufficientStatistic] { this: T=>
  def +(t: T):T
  def *(weight: Double):T
}