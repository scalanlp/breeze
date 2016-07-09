package breeze.stats.distributions

import breeze.numerics.log
import breeze.optimize.DiffFunction
import org.apache.commons.math3.distribution.ExponentialDistribution

import scala.runtime.ScalaRunTime

/**
 *
 * @author dlwh
 */

case class Exponential(rate: Double)(implicit basis: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] with HasCdf with HasInverseCdf {
  override def toString() = ScalaRunTime._toString(this)
  require(rate > 0)

  def unnormalizedLogPdf(x: Double) = - rate * x

  lazy val logNormalizer = - math.log(rate)

  def draw() = -math.log(basis.uniform.draw())/rate

  override def probability(x: Double, y: Double): Double = {
    new ExponentialDistribution(rate).probability(x, y)
  }

  override def inverseCdf(p: Double): Double = {
    new ExponentialDistribution(rate).inverseCumulativeProbability(p)
  }

  // Probability that x < a <= Y
  override def cdf(x: Double): Double = {
    new ExponentialDistribution(rate).cumulativeProbability(x)
  }

  override def mean: Double = 1 / rate

  override def variance: Double = 1 / (rate * rate)

  override def mode: Double = 0

  override def entropy: Double = 1 - log(rate)
}

object Exponential extends ExponentialFamily[Exponential,Double] with ContinuousDistributionUFuncProvider[Double,Exponential] {
  type Parameter = Double
  case class SufficientStatistic(n: Double, v: Double) extends breeze.stats.distributions.SufficientStatistic[SufficientStatistic] {
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
