package scalanlp.stats.sampling

import Math._;

import scalanlp.math.Numerics.lgamma;
import scalanlp.counters._;
import Counters._

class Beta(a: Double, b: Double) extends ContinuousDistr[Double] 
    with ConjugatePrior[Double, Boolean] with Moments[Double] {
  require(a > 0.0);
  require(b > 0.0);

  override def unnormalizedLogPdf(x: Double) = {
    require(x >= 0);
    require(x <= 1)
    (a-1) * log(x) + (b-1) * log(1-x)
  }
  
  val logNormalizer =  lgamma(a) + lgamma(b) - lgamma(a+b)
  
  private val aGamma = new Gamma(a,1);
  private val bGamma = new Gamma(b,1.);
  
  override def draw() = {
    val ad = aGamma.get;
    val bd = bGamma.get;
    (ad) / (ad + bd);
  }
  
  override def posterior(obs: Iterator[(Boolean,Int)]) = {
    val ctr = Counters.aggregate(obs.map{ case (k,v) => (k,v.toDouble)});
    new Beta(ctr(true)+a, ctr(false)+b)
  }
  
  def predictive = {
    val ctr = DoubleCounter[Boolean]();
    ctr(true) = a;
    ctr(false) = b;
    new Polya(ctr);
  }

  def mean = a / (a + b);
  def variance = (a * b) / ( (a + b) * (a+b) * (a+b+1));
  

}
