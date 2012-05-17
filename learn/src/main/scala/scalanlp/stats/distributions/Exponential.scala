package scalanlp.stats.distributions

import runtime.ScalaRunTime
import scalanlp.optimize.DiffFunction

/**
 * 
 * @author dlwh
 */

case class Exponential(rate: Double)(implicit basis: RandBasis=Rand) extends ContinuousDistr[Double] {
  override def toString() = ScalaRunTime._toString(this)
  require(rate > 0)

  def unnormalizedLogPdf(x: Double) = - rate * x

  val logNormalizer = - math.log(rate)

  def draw() = { for {
    x <- basis.uniform
  } yield - math.log(x) / rate} get
}

object Exponential extends ExponentialFamily[Exponential,Double] {
  type Parameter = Double
  case class SufficientStatistic(n: Double, v: Double) extends scalanlp.stats.distributions.SufficientStatistic[SufficientStatistic] {
    def +(t: SufficientStatistic) = copy(n + t.n, v + t.v)

    def *(weight: Double) = copy(n * weight, v * weight)
  }

  def emptySufficientStatistic = SufficientStatistic(0,0)

  def sufficientStatisticFor(t: Double) = SufficientStatistic(1,t)

  def mle(stats: SufficientStatistic) = {
    stats.n / stats.v
  }

  def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[Double] {
    def calculate(x: Double) = {
      val obj = x  * stats.v - stats.n * math.log(x)
      val deriv = stats.v - stats.n / x
      (obj,deriv)
    }
  }

  def distribution(p: Double) = new Exponential(p)
}
