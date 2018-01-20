package breeze.optimize.linear

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * mixed 0-1 ILP Solver based on Branch and bound
 *
 * @see http://www.ee.ucla.edu/ee236a/lectures/intlp.pdf
 * @author dlwh
 */
object BranchAndBound {

  private case class State(lp_value: Double, remaining: Seq[Int])

  def minimize(
      A: DenseMatrix[Double],
      b: DenseVector[Double],
      c: DenseVector[Double],
      x0: DenseVector[Double],
      integers: Seq[Int]) = {}

}
