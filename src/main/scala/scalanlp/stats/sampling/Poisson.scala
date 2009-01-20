package scalanlp.stats.sampling;
import math.Numerics._;
import Math._;

class Poisson(val mean: Double) extends DiscreteDistr[Int] {
  private val ell = Math.exp(-mean);
  //  TODO: this is from Knuth, but it's linear in mean.
  def draw() = {
    var k = 0;
    var p = 1.;
    do { 
      k += 1;
      p *= Rand.uniform.get();
    } while(p >= ell);
    k - 1;
  }

  def probabilityOf(k:Int) = Math.exp(logProbabilityOf(k));
  override def logProbabilityOf(k:Int) = {
    -mean + k * log(mean) - lgamma(k);
  }
}
