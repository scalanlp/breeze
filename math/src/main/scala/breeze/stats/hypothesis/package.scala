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
    val MeanAndVariance(mu1, var1, n1) = meanAndVariance(it1)
    //second sample
    val MeanAndVariance(mu2, var2, n2) = meanAndVariance(it2)
    require(var1 > 0 && var2 > 0, "Two Sample T Test requires that both"
        +"samples have variance > 0")
    val tScore = (mu1 - mu2) / sqrt( (var1 / n1) + (var2 / n2) ) // T statistic
    val dof = pow((var1/n1) + (var2/n2), 2) / ( pow(var1,2)/( pow(n1,2)*(n1-1) ) +
        pow(var2,2)/(pow(n2,2)*(n2-1)) ) //Welch–Satterthwaite equation
    new StudentsT(dof)(RandBasis.mt0).unnormalizedPdf(tScore) //return p value
  }

  def tTest[T](it1: Traversable[T])(implicit numeric: Numeric[T]):Double = tTest(it1.map(numeric.toDouble))
  def tTest(it1: Traversable[Double]):Double = tTestImpl(it1) // Specialized case of tTest[T](it1: Traversable[T])(implicit numeric: Numeric[T]) - if we don't specialize to Traversable[Double] then it will stack overflow as it repeatedly calls itself.
  def tTest[X](it1: X)(implicit ct:CanTraverseValues[X,Double]):Double = tTestImpl(it1)

  private def tTestImpl[X](it1: X)(implicit ct:CanTraverseValues[X,Double]):Double = {
    val MeanAndVariance(mu1, var1, n1) = meanAndVariance(it1)
    val Z = mu1 / sqrt( var1/n1 )
    val dof = n1-1
    new StudentsT(dof)(RandBasis.mt0).unnormalizedPdf(Z) //return p value
  }


}
