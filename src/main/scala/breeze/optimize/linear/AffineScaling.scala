package breeze.optimize.linear

import breeze.linalg._
import breeze.util.SerializableLogging


/**
 * Simple LP solver based on http://en.wikipedia.org/wiki/Karmarkar's_algorithm
 * Note that this is not Karmarkar's algorithm.
 * @author dlwh
 */
class AffineScaling extends SerializableLogging {
  /**
   * Maximize c dot x s.t. Ax <= b
   */
  def maximize(A: DenseMatrix[Double],
               b: DenseVector[Double],
               c: DenseVector[Double],
               x0: DenseVector[Double],
               gamma:Double = 0.5,
               eps: Double = 1E-5) = {
    var converged = false
    var x = x0
    var cv = x dot c
    while(!converged) {
      val vk = b - A * x
      val D = diag(vk :^ -2.0)
      val hx = (A.t * D * A).asInstanceOf[DenseMatrix[Double]] \ c
      val hv:DenseVector[Double] = A * hx * -1.0
      if(hv.values.exists(_ >= 0)) throw UnboundedProblem

      val constraints = (for(i <- 0 until hv.length if hv(i) < 0) yield (- vk(i)/hv(i)))
      val alpha = if(constraints.size > 1) constraints.min * gamma else 0.0
      val xn = x + hx * alpha
      val cvn = xn dot c
      logger.info("Current obj: " + cvn)
      if( (cvn - cv).abs/(1.0 max cvn) < eps) converged = true
      cv = cvn
      x = xn
    }
    x
  }

  object UnboundedProblem extends Exception
}

object AffineScaling extends AffineScaling