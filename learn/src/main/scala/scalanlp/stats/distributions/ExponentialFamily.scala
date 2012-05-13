package scalanlp
package stats
package distributions

import optimize.DiffFunction

/**
 * 
 * @author dlwh
 */

trait ExponentialFamily[D,T]  {
  type Parameter;
  type SufficientStatistic <: distributions.SufficientStatistic[SufficientStatistic];
  def emptySufficientStatistic:SufficientStatistic;
  def sufficientStatisticFor(t: T):SufficientStatistic;
  def mle(stats: SufficientStatistic):Parameter
  def likelihoodFunction(stats: SufficientStatistic):DiffFunction[Parameter]
  def distribution(p: Parameter):D;
}