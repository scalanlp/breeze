package breeze.stats
import scala.math.pow
import scala.math.sqrt
import breeze.stats.distributions._
import breeze.linalg.support._
import CanTraverseValues._

/**
 * This package contains hypothesis tests.
 *
 */
package object hypothesis {

  /**
   * Implements two tailed Welch's T Test (equivalent to t.test in R)
   * Returns a p value
   */
  def tTest[T](it1: TraversableOnce[T],it2 : Traversable[T])(implicit numeric: Numeric[T]): Double = tTest[TraversableOnce[Double]](it1.map(numeric.toDouble), it2.map(numeric.toDouble)) //explicit type annotation ensures that the compiler runs the CanTraverseValues implementation of it

  def tTest[X](it1 : X, it2 : X)(implicit ct: CanTraverseValues[X,Double]): Double = {
    //first sample
    val res1 = meanAndVariance(it1)
    val mu1 = res1._1.toDouble;
    val var1 = res1._2.toDouble //Convert to doubles because sqrt
    val N1 = res1._3
    //second sample
    val res2 = meanAndVariance(it2)
    val mu2 = res2._1.toDouble; val var2 = res2._2.toDouble
    val N2 = res1._3
    require(var1 > 0 && var2 > 0, "Two Sample T Test requires that both"
        +"samples have variance > 0")
    val tScore = (mu1 - mu2).toDouble / sqrt( ((var1/N1) + (var2/N2)) ) // T statistic
    val dof = pow((var1/N1) + (var2/N2), 2) / ( pow(var1,2)/( pow(N1,2)*(N1-1) ) +
        pow(var2,2)/(pow(N2,2)*(N2-1)) ) //Welch–Satterthwaite equation
    new StudentsT(dof)(RandBasis.mt0).unnormalizedPdf(tScore) //return p value
  }

  def tTest[T](it1: Traversable[T])(implicit numeric: Numeric[T]):Double = tTest(it1.map(numeric.toDouble))
  def tTest[X](it1: X)(implicit ct:CanTraverseValues[X,Double]):Double = {
    val res1 = meanAndVariance(it1)
    val mu1 = res1._1.toDouble;
    val var1 = res1._2.toDouble //Convert to doubles because sqrt
    val N1 = res1._3
    val Z = mu1 / sqrt( var1/N1 )
    val dof = N1-1
    new StudentsT(dof)(RandBasis.mt0).unnormalizedPdf(Z) //return p value
  }
}
