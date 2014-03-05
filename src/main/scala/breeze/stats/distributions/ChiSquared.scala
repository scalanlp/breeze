package breeze.stats.distributions

import scala.runtime.ScalaRunTime
import breeze.optimize.DiffFunction
import breeze.numerics._

/**
 * Chi-Squared distribution with k degrees of freedom.
 *
 * @author dlwh
 **/
case class ChiSquared(k: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] {
  private val innerGamma = Gamma(k/2, 2)


  def draw(): Double = innerGamma.draw()

  def unnormalizedLogPdf(x: Double): Double = innerGamma.unnormalizedLogPdf(x)

  val logNormalizer: Double = innerGamma.logNormalizer

  def mean: Double = innerGamma.mean
  def variance: Double = innerGamma.variance
  def mode: Double = innerGamma.mode
  def entropy: Double = innerGamma.entropy

  override def toString: String = ScalaRunTime._toString(this)
}

object ChiSquared extends ExponentialFamily[ChiSquared, Double] with ContinuousDistributionUFuncProvider[Double,ChiSquared] {
  type Parameter = Double
  type SufficientStatistic = Gamma.SufficientStatistic
  def emptySufficientStatistic: ChiSquared.SufficientStatistic = Gamma.emptySufficientStatistic

  def sufficientStatisticFor(t: Double): ChiSquared.SufficientStatistic = Gamma.sufficientStatisticFor(t)


  def likelihoodFunction(stats: ChiSquared.SufficientStatistic): DiffFunction[ChiSquared.Parameter] = {
    val inner = Gamma.likelihoodFunction(stats)
    new DiffFunction[ChiSquared.Parameter] {
      def calculate(x: ChiSquared.Parameter): (Double, ChiSquared.Parameter) = {
        val (obj, ggrad) = inner.calculate((x/2.0, 2.0))
        obj -> ggrad._1
      }
    }
  }

  def mle(ss: ChiSquared.SufficientStatistic): ChiSquared.Parameter = {
    ss.mean
  }

  def distribution(p: ChiSquared.Parameter): ChiSquared = ChiSquared(p)
}
