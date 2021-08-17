package breeze.stats.distributions

import breeze.optimize.DiffFunction

import scala.runtime.ScalaRunTime

/**
 * Chi-Squared distribution with k degrees of freedom.
 *
 * @author dlwh
 **/
case class ChiSquared(k: Double)(implicit rand: RandBasis)
    extends ContinuousDistr[Double]
    with Moments[Double, Double]
    with HasCdf
    with HasInverseCdf {
  private val innerGamma = Gamma(k / 2, 2)

  def draw(): Double = innerGamma.draw()

  override def pdf(x: Double): Double =
    if (x > 0.0) {
      math.exp(logPdf(x))
    } else if (x == 0.0) {
      if (k > 2.0) {
        0.0
      } else if (k == 2.0) {
        0.5
      } else {
        Double.PositiveInfinity
      }
    } else {
      throw new IllegalArgumentException("Domain of ChiSquared.pdf is [0,Infinity), you tried to apply to " + x)
    }

  def unnormalizedLogPdf(x: Double): Double = innerGamma.unnormalizedLogPdf(x)

  lazy val logNormalizer: Double = innerGamma.logNormalizer

  def mean: Double = innerGamma.mean
  def variance: Double = innerGamma.variance
  def mode: Double = innerGamma.mode
  def entropy: Double = innerGamma.entropy

  override def toString: String = ScalaRunTime._toString(this)

  override def probability(x: Double, y: Double): Double = {
    innerGamma.probability(x, y)
  }

  override def inverseCdf(p: Double): Double = {
    innerGamma.inverseCdf(p)
  }

  // Probability that x < a <= Y
  override def cdf(x: Double): Double = {
    innerGamma.cdf(x)
  }
}

object ChiSquared
    extends ExponentialFamily[ChiSquared, Double]
    with ContinuousDistributionUFuncProvider[Double, ChiSquared] {
  type Parameter = Double
  type SufficientStatistic = Gamma.SufficientStatistic
  def emptySufficientStatistic: ChiSquared.SufficientStatistic = Gamma.emptySufficientStatistic

  def sufficientStatisticFor(t: Double): ChiSquared.SufficientStatistic = Gamma.sufficientStatisticFor(t)

  def likelihoodFunction(stats: ChiSquared.SufficientStatistic): DiffFunction[ChiSquared.Parameter] = {
    val inner = Gamma.likelihoodFunction(stats)
    new DiffFunction[ChiSquared.Parameter] {
      def calculate(x: ChiSquared.Parameter): (Double, ChiSquared.Parameter) = {
        val (obj, ggrad) = inner.calculate((x / 2.0, 2.0))
        obj -> ggrad._1
      }
    }
  }

  def mle(ss: ChiSquared.SufficientStatistic): ChiSquared.Parameter = {
    ss.mean
  }

  override def distribution(p: ChiSquared.Parameter)(implicit rand: RandBasis): ChiSquared = ChiSquared(p)
}
