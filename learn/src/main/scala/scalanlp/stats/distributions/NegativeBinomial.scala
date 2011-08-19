package scalanlp.stats.distributions

import scalala.library.Numerics.lgamma

/**
 * Negative Binomial Distribution
 * @param r number of failures until stop
 * @param p prob of success
 * @author dlwh
 */
case class NegativeBinomial(r: Double, p: Double) extends DiscreteDistr[Int] {
  private val gen = for {
    lambda <- Gamma(r,p / (1-p))
    i <- Poisson(lambda)
  } yield i
  def draw() = gen.draw()

  def probabilityOf(x: Int) = logProbabilityOf(x)

  override def logProbabilityOf(k: Int) = {
    lgamma(r + k) - lgamma(k+1) - lgamma(r) + r * math.log(1-p) + k * math.log(p)
  }
}