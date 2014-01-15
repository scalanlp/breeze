package breeze.stats.distributions

import breeze.numerics._

/**
 * 
 * @author dlwh
 */

case class Uniform(low: Double, high: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double] with Moments[Double, Double] {
  require(low <= high, "low <= high")
  def draw() = rand.uniform.get * (high - low) + low

  def unnormalizedLogPdf(x: Double) = {
    logI(x >= low && x <= high)
  }

  def logNormalizer = entropy

  def mode = mean

  def mean = (low + high)/2
  def variance = math.pow(high - low,2)/12
  def entropy = math.log(high - low)
}