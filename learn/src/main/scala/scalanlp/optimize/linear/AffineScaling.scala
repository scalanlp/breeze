package scalanlp.optimize.linear

import scalala.Scalala._
import scalala.tensor.dense.{DenseMatrix, DenseVector}


/**
 * Simple LP solver based on http://en.wikipedia.org/wiki/Karmarkar's_algorithm
 * Note that this is not Karmarkar's algorithm.
 * @author dlwh
 */
object AffineScaling {
  /**
   * Maximize c dot x s.t. Ax <= b
   */
  def maximize(A: DenseMatrix, b: DenseVector, c: DenseVector, x0: DenseVector, gamma:Double = 0.5, eps: Double = 1E-5) = {
    var converged = false;
    var x = x0;
    var cv = x dot c;
    while(!converged) {
      val vk = b - A * x value;
      val D = diag(vk :^ -2 value);
      val hx = (A.t * D * A) \ c value;
      val hv = -A * hx value;
      if(hv.activeValues.exists(_ >= 0)) error("unbounded");

      val alpha = gamma * (for(i <- 0 until hv.size if hv(i) < 0) yield (- vk(i)/hv(i))).min;
      val xn = x + hx * alpha value;
      val cvn = xn dot c
      if( (cvn - cv).abs/(1.0 max cvn) < eps) converged = true;
      cv = cvn;
      x = xn;
    }
    x;
  }
}