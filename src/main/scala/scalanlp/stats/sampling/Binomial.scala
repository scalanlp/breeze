package scalanlp.stats.sampling

import math.Numerics.lgamma;
import Math._;

class Binomial(n: Int, p: Double) extends DiscreteDistr[Int] {
  require(n > 0);
  require(p > 0.0);
  def probabilityOf(k: Int) = exp(logProbabilityOf(k));
  
  override def logProbabilityOf(k: Int) = {
    require(n >= k);
    require(k >= 0);
    lgamma(n+1) - lgamma(k+1) - lgamma(n-k+1) + k * log(p) + (n-k) * log(n-k)
  }
  
  override def draw() = {
    (1 to n).map(_ => if(Rand.uniform.get < p) 1 else 0).foldLeft(0)(_+_);
  }

}
