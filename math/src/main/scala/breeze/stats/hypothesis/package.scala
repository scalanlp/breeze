package breeze.stats
import scala.math.pow
import scala.math.sqrt
import breeze.stats.distributions._
import breeze.linalg.support._
import CanTraverseValues._
import breeze.stats.meanAndVariance.MeanAndVariance

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

  def tTest[T](it1: Traversable[T])(implicit numeric: Numeric[T]):Double = tTest[TraversableOnce[Double]](it1.map(numeric.toDouble)) //explicit type annotation ensures that the compiler runs the CanTraverseValues implementation of it
  def tTest[X](it1: X)(implicit ct:CanTraverseValues[X,Double]):Double = {
    val MeanAndVariance(mu1, var1, n1) = meanAndVariance(it1)
    val Z = mu1 / sqrt( var1/n1 )
    val dof = n1-1
    new StudentsT(dof)(RandBasis.mt0).unnormalizedPdf(Z) //return p value
  }

  case class Chi2Result(chi2: Double, pVal: Double)
  private def chiSquaredTerm(e: Double, o: Double) = (e-o)*(e-o)/e

  def chi2Test(successControl: Long, trialsControl: Long, successVariant: Long, trialsVariant: Long):Chi2Result = {
    /*
     * Takes 2 Bernoulli trials, and determines using a chi2 test whether there is a
     * statistically significant difference between the probabilities of success
     * between control and a variant.
     */
    val meanP = (successControl + successVariant).toDouble / (trialsControl + trialsVariant).toDouble
    val chi2 = (chiSquaredTerm(meanP * trialsControl, successControl) + chiSquaredTerm((1-meanP) * trialsControl, trialsControl - successControl)
              + chiSquaredTerm(meanP * trialsVariant, successVariant) + chiSquaredTerm((1-meanP) * trialsVariant, trialsVariant - successVariant))
    val pVal = 1.0 - Gamma(0.5, 2.0).cdf(chi2)
    Chi2Result(chi2, pVal)
  }

  /**
    * Takes a sequence of N Bernoulli trials, and determines using a chi2 test whether there is a
    * statistically significant difference between the N variants and a control.
    * I.e., the variants may differ from each other, but this only determines whether
    * they differ from control.
    *
    * The pVal reported in the results is the probability (assuming the null hypothesis) of
    * a false positive at least this large in *any* variant, not in one particular variant.
    * I.e., multiple comparisons are corrected for.
    */
  def chi2Test(control: (Long, Long), trials: Seq[(Long, Long)]): Seq[Chi2Result] = {
    val numTrials = trials.size
    trials.map( x => chi2Test(control._1, control._2, x._1, x._2) ).map(r => Chi2Result(r.chi2, sidakCorrectedPVal(r.pVal, numTrials)))
  }

  /**
    * Takes a p-value run for a single statistical test,
    * and then corrects for multiple comparisons.
    *
    * I.e., if you run n tests with a p-value cutoff of 5%
    * yielding p-values p1, p2, ..., pn, then if
    * sidakCorrectedPVal(p1,n) < 5% or sidakCorrectedPVal(p2, n) < 5%, etc,
    * you can reject the null hypothesis.
    */
  def sidakCorrectedPVal(p: Double, n: Int): Double = {
    1.0 - pow(1.0-p, n)
  }

  /**
    * Takes a p-value run for a single statistical test,
    * and then corrects for multiple comparisons.
    *
    * This function is the inverse of sidakCorrectedPVal.
    * If you run n tests and want a 5% chance of false positive (assuming null
    * hypothesis is true) across *all* tests, then you can
    * run each individual test with a p-value cutoff of
    * sidakCorrectedPValCutoff(0.05, n).
    */
  def sidakCorrectedPValCutoff(p: Double, n: Int): Double = {
    1.0 - pow(1.0-p, 1.0/n.toDouble)
  }
}
