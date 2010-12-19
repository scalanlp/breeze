package scalanlp.optimize.linear

import scalala.tensor.dense.{DenseMatrix, DenseVector}
import scalala.library.LinearAlgebra.diag;


/**
 * Simple LP solver based on http://en.wikipedia.org/wiki/Karmarkar's_algorithm
 * Note that this is not Karmarkar's algorithm.
 * @author dlwh
 */
object AffineScaling {
  /**
   * Maximize c dot x s.t. Ax <= b
   */
  def maximize(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x0: DenseVector[Double], gamma:Double = 0.5, eps: Double = 1E-5) = {
    var converged = false;
    var x = x0.asCol;
    var cv = x dot c;
    while(!converged) {
      val vk = b.asCol - A * x;
      val D = diag(vk :^ -2);
      val hx = (A.t * D * A).asInstanceOf[DenseMatrix[Double]] \ c.asCol;
      val hv:DenseVector[Double] = A * hx * -1.0;
      if(hv.values.exists(_ >= 0)) error("unbounded");

      val alpha = gamma * (for(i <- 0 until hv.length if hv(i) < 0) yield (- vk(i)/hv(i))).min;
      val xn = x + hx * alpha;
      val cvn = xn dot c
      if( (cvn - cv).abs/(1.0 max cvn) < eps) converged = true;
      cv = cvn;
      x = xn;
    }
    x;
  }
}