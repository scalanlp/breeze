package breeze.stats
import scala.math.pow
import scala.math.sqrt
import breeze.stats.distributions._

/**
 * This package contains hypothesis tests. 
 * 
 */
package object hypothesis {  
  
  /**
   * Implements two tailed Welch's T Test (equivalent to t.test in R)
   * Returns a p value
   */
  def tTest[T](it1 : TraversableOnce[T], it2 : Option[Traversable[T]] = None)(implicit frac: Fractional[T]) = {
    import frac.mkNumericOps
    val res1 = DescriptiveStats.meanAndVariance(it1)
    val mu1 = res1._1.toDouble; 
    val var1 = res1._2.toDouble //Convert to doubles because sqrt
    val N1 = res1._3 
    val (tScore,degreesOfFreedom) = it2 match { //one sample test
      case None => {
        val Z = mu1 / sqrt( var1/N1 )
        val dof = N1-1
        (Z,dof.toDouble)
      }
      case Some(it) => { //two sample test
        val res2 = DescriptiveStats.meanAndVariance(it)
        val mu2 = res2._1.toDouble; val var2 = res2._2.toDouble
        val  N2 = res1._3 
        require(var1 > 0 && var2 > 0, "Two Sample T Test requires that both"
            +"samples have variance > 0")
        
        val T = (mu1 - mu2).toDouble / sqrt( ((var1/N1) + (var2/N2)) ) // T statistic
        val dof = pow((var1/N1) + (var2/N2), 2) / ( pow(var1,2)/( pow(N1,2)*(N1-1) ) +
            pow(var2,2)/(pow(N2,2)*(N2-1)) ) //Welch–Satterthwaite equation
        (T,dof)
      }
    }
    val tdist = new StudentsT(degreesOfFreedom)(RandBasis.mt0)
    tdist.unnormalizedPdf(tScore) //return p value
  }
}