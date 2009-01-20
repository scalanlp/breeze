package scalanlp.stats.sampling;

import Rand._;
import Math._;

class Gaussian(val mu :Double, val sigma : Double) extends ContinuousDistr[Double] {
  private val inner = Rand.gaussian(mu,sigma);
  def draw() = inner.get();

  def pdf(t : Double) = exp(logPdf(t));
  override def logPdf(t :Double) =  unnormalizedLogPdf(t) + logNormalizer;
  override def unnormalizedLogPdf(t: Double) = { 
    val d = (t - mu)/sigma; 
    d *d 
  } 
  override def unnormalizedPdf(t: Double) = exp(unnormalizedLogPdf(t));
  
  val normalizer = 1.0/sqrt(2 * Pi) / sigma;
  val logNormalizer = log(1.0/sqrt(2 * Pi)) - log(sigma);
}
