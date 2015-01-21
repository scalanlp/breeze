package breeze.stats.distributions

import scala.runtime.ScalaRunTime
import org.apache.commons.math3.distribution.TDistribution
import breeze.numerics._

/**
 * Implements Student's T distribution
 * [[http://en.wikipedia.org/wiki/Student's_t-distribution]]
 *
 * @author dlwh
 **/
case class StudentsT(degreesOfFreedom: Double)(implicit randBasis: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] with HasCdf {
  require(degreesOfFreedom > 0, "degreesOfFreedom must be positive, but got " + degreesOfFreedom)
  override def toString: String = ScalaRunTime._toString(this)

  private val innerInstance = new TDistribution(randBasis.generator, degreesOfFreedom, TDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)

  def draw(): Double = {
    // TODO: for small DoF this seems a little wrong..., StudentsT is even worse though.
    // numpy version
    val N = randBasis.gaussian.draw()
    val G = new Gamma(degreesOfFreedom/2, 1.0).draw()
    val X = sqrt(degreesOfFreedom/2)*N/sqrt(G)
    X
  }


  override def probability(x: Double, y: Double): Double = {
    val t = new org.apache.commons.math3.distribution.TDistribution(degreesOfFreedom)
    t.probability(x, y)
  }

  def cdf(x: Double):Double = {
    val t = new org.apache.commons.math3.distribution.TDistribution(degreesOfFreedom)
    t.cumulativeProbability(x)
  }


  def unnormalizedLogPdf(x: Double): Double = -(degreesOfFreedom  + 1)/2 * math.log(1 + (x * x)/degreesOfFreedom)

  lazy val logNormalizer: Double = math.sqrt(math.Pi * degreesOfFreedom) + lgamma((degreesOfFreedom / 2) - lgamma(degreesOfFreedom + 1)/2)

  def mean: Double = innerInstance.getNumericalMean

  def variance: Double = innerInstance.getNumericalVariance

  def entropy: Double = (
    (degreesOfFreedom + 1)/2 * (digamma((degreesOfFreedom + 1)/2) -  digamma(degreesOfFreedom))
      - .5 * log(degreesOfFreedom)
      + lbeta(degreesOfFreedom/2, 0.5)
  )

  def mode: Double = mean
}

object StudentsT extends ContinuousDistributionUFuncProvider[Double,StudentsT]
