package scalanlp.stats.sampling

import scalanlp.math.Numerics.lgamma;
import Math._;

class Bernoulli(p: Double) extends DiscreteDistr[Boolean] {
  require(p > 0.0);
  def probabilityOf(b: Boolean) = if(b) p else (1-p)
  
  override def draw() = {
    Rand.uniform.get < p
  }

}
