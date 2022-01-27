package breeze.stats.distributions

import breeze.macros._
import breeze.numerics._

/**
 *
 * @author dlwh
 */
case class Uniform(low: Double, high: Double)(implicit rand: RandBasis)
    extends ContinuousDistr[Double]
    with Moments[Double, Double]
    with HasCdf
    with HasInverseCdf {
  require(low < high)
  def draw(): Double = rand.uniform.draw() * (high - low) + low

  def unnormalizedLogPdf(x: Double): Double = {
    logI(x >= low && x <= high)
  }

  lazy val logNormalizer: Double = entropy

  def mode: Double = mean

  def mean: Double = (low + high) / 2
  def variance: Double = math.pow(high - low, 2) / 12
  def entropy: Double = math.log(high - low)

  def cdf(x: Double): Double = {
    if (x <= low) 0.0
    else if (x >= high) 1.0
    else (x - low) / (high - low)
  }

  override def probability(x: Double, y: Double): Double = {
    (y - x) / (high - low)
  }

  override def inverseCdf(p: Double): Double = (high - low) * p + low
}

object Uniform extends ContinuousDistributionUFuncProvider[Double, Uniform]
