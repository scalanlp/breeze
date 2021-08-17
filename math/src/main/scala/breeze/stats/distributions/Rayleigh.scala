package breeze.stats.distributions

import breeze.numerics.constants.{Pi, γ}
import breeze.numerics.{expm1, log, sqrt}

case class Rayleigh(scale: Double)(implicit rand: RandBasis)
    extends ContinuousDistr[Double]
    with Moments[Double, Double]
    with HasCdf {
  def mean: Double = scale * sqrt(Pi / 2)

  def mode: Double = scale

  def variance: Double = (4 - Pi) / 2 * scale * scale

  def entropy: Double = log(scale / sqrt(2)) + γ / 2 + 1

  def logNormalizer: Double = 2 * math.log(scale)

  def draw(): Double = {
    // from numpy
    math.sqrt(2 * new Exponential(1).draw() * scale * scale)
  }

  def unnormalizedLogPdf(x: Double): Double = {
    if (x <= 0.0) return Double.NegativeInfinity
    log(x) - x * x / (2 * scale * scale)
  }

  def cdf(x: Double): Double = {
    val xs = x / scale
    -expm1(-(xs * xs) / 2)
  }

  override def probability(x: Double, y: Double): Double = {
    cdf(y) - cdf(x)
  }
}
