package scalanlp.stats.sampling

import scalanlp.math.Numerics.lgamma;
import Math._;

class Bernoulli(p: Double) extends DiscreteDistr[Boolean] with Moments[Double] {
  require(p >= 0.0);
  require(p <= 1.0);
  def probabilityOf(b: Boolean) = if(b) p else (1-p)
  
  override def draw() = {
    Rand.uniform.get < p
  }

  def mean = p;
  def variance = p * (1-p);
}
