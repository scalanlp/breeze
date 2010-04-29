package scalanlp.stats.sampling

import scalanlp.math.Numerics.lgamma;
import Math._;

/*
 * A Bernoulli distribution represents a distribution over weighted coin flips, with p(true) = p, the
 *
 */
class Bernoulli(p: Double, rand: RandBasis = Rand) extends DiscreteDistr[Boolean] with Moments[Double] {
  require(p >= 0.0);
  require(p <= 1.0);
  def probabilityOf(b: Boolean) = if(b) p else (1-p)
  
  override def draw() = {
    rand.uniform.get < p
  }

  def mean = p;
  def variance = p * (1-p);
}
