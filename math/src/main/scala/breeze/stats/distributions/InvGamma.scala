package breeze.stats
package distributions

import breeze.numerics.{digamma, lgamma}

case class InvGamma(shape: Double, scale: Double)(implicit val basis: RandBasis) extends ContinuousDistr[Double]
  with Moments[Double, Double]
  with HasCdf {

  def unnormalizedLogPdf(x: Double): Double = {
    math.log(x) * (-shape - 1) - scale / x
  }

  lazy val logNormalizer: Double = {
    shape * math.log(scale) - lgamma(shape)
  }

  def mean: Double = {
    require(shape > 1)
    scale / (shape - 1)
  }

  def variance: Double = {
    require(shape > 2)
    scale * scale / ((shape - 1) * (shape - 1) * (shape - 2))
  }

  def entropy: Double = {
    shape + math.log(scale) + lgamma(shape) - (1 + shape) * digamma(shape)
  }

  def mode: Double = scale / (shape + 1)

  def probability(x: Double, y: Double): Double = cdf(y) - cdf(x)

  def cdf(x: Double): Double = {
    1 - math.exp(lgamma(shape, scale / x) - lgamma(shape))
  }

  private val innerGamma = Gamma(shape, 1/scale)
  def draw(): Double = {
    1.0 / innerGamma.draw()
  }
}
