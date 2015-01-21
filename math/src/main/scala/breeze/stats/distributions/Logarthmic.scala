package breeze.stats.distributions

import runtime.ScalaRunTime
import breeze.numerics.{log, round, log1p, expm1}

/**
 * The Logarithmic distribution
 *
 * http://en.wikipedia.org/wiki/Logarithmic_distribution
 * @author dlwh
 */
case class Logarthmic(p: Double)(implicit rand: RandBasis=Rand) extends DiscreteDistr[Int] with Moments[Double, Double] {
  require(p >= 0)
  require(p <= 1)

  // from Efficient Generation of Logarithmically Distributed Pseudo-Random Variables
  private val h = log1p(-p)


  def draw() = {

    val u2 = rand.uniform.draw()

    if(u2 > p) {
      1
    } else {
      val u1 = rand.uniform.draw()
      val q = -expm1(u1 * h)
      if(u2 < q * q) {
        round(1.0 + log(u2)/log(q)).toInt
      } else if (u2 > q) {
        1
      } else {
        2
      }
    }


  }

  def probabilityOf(x: Int) = {
    -1.0/log1p(-p) * math.pow(p,x)/x
  }

  def mean = -1.0/log1p(-p) * (p/(1-p))

  def variance = {
    val l1p = log1p(-p)
    val onemp = 1 - p
    val denompart = onemp * l1p
    -p * (p + l1p)/(denompart * denompart)
  }

  def mode = 1
  def entropy = ???

  override def toString() = ScalaRunTime._toString(this)

}

