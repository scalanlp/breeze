package breeze.stats.distributions

import breeze.numerics.{pow, exp, log}

/**
 * http://en.wikipedia.org/wiki/Laplace_distribution
 *
 * @author dlwh
 **/
case class Pareto(scale: Double, shape: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] with HasCdf {
  def mean: Double = {
    if(shape <= 1) {
      Double.PositiveInfinity
    } else {
      scale * shape / (shape - 1)
    }
  }

  def mode: Double = scale

  def variance: Double = {
    if(shape <= 1) throw new IllegalArgumentException("undefined variance for shape < 1")
    else if(shape <= 2) Double.PositiveInfinity
    else {
      pow(scale/(shape - 1.0), 2.0) * shape /(shape - 2.0)
    }
  }

  def entropy: Double = {
    log(scale/shape) + 1.0/shape + 1.0
  }

  lazy val logNormalizer: Double = 1/(shape * math.pow(scale, shape))

  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def draw(): Double = {
    val e = new Exponential(shape)
    exp(e.draw()) * scale
  }

  def unnormalizedLogPdf(x: Double): Double = {
    -(shape + 1) * log(x)
  }

  def probability(x: Double, y: Double): Double = {
    cdf(y) - cdf(x)
  }

  private def cdf(x: Double) = x match {
    case x if x < scale => 0.0
    case Double.PositiveInfinity => 1.0
    case x =>
      1 - math.pow(scale / x, shape)
  }

}
