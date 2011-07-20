package scalanlp.optimize
package linear

import scalala.tensor.Vector;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._
import scalala.tensor.dense.{DenseVectorCol, DenseVector, DenseMatrix}
;

/**
 * InteriorPoint solver for LPs.
 * @author dlwh
 */
object InteriorPoint {
  val TOLERANCE = 1E-4
  val tau = 0.9
  /**
   *
   * Solves the LP:
   *
   * minimize_x c * x
   * subject to A * x <= b and x >= 0
   *
   * with initial feasible point x0
   *
   * @see http://www.doc.ic.ac.uk/~br/berc/pdiplin.pdf
   * @see http://www.ee.ucla.edu/ee236a/lectures/mpc.pdf
   */
  def minimize(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x0: DenseVector[Double]) = {
    val m = A.numRows;
    val n = A.numCols;
    val x = DenseVector.zeros[Double](n)
//    assert((A * x0.asCol - b).valuesIterator.forall(_ <= 0));
    x += x0;
    val s = DenseVector.zeros[Double](m);
    s += 1;
    val z = DenseVector.zeros[Double](m);
    z += 1;
//    assert((A.t * z + s - c).valuesIterator.forall(_.abs <= 1E-4));
    var converged = false;
    while(!converged) {
      val (zAff,xAff,sAff) = computeAffineScalingDir(A,b.asCol,c.asCol,x,s,z);
      val scaleX = lineSearch(s,sAff);
      val scaleZ = lineSearch(z,zAff);
      val sigma = math.pow( (s + sAff * scaleX).dot(z + zAff * scaleZ)/(s dot z) ,3);
      val (zCC,xCC,sCC) = computeCenteringCorrectorDir(A,b.asCol,c.asCol,x,s,z,sAff,zAff,sigma);

      val dz = zAff + zCC;
      val dx = xAff + xCC;
      val ds = sAff + sCC;

      val scaleXF = lineSearch(s,ds);
      val scaleZF = lineSearch(z,dz);

      x += dx * (.99 * scaleXF)
      s += ds * (.99 * scaleXF)
      z += dz * (.99 * scaleZF)

      val gap = (c dot x) + (b dot z);
      converged = gap.abs < 1E-6
//      assert(gap > -1E-3,gap);
    }

    x
  }

  private def lineSearch(x: DenseVector[Double],dx: Vector[Double]):Double = {
    var alpha = 1.0;
    while((x + dx*alpha).valuesIterator.exists(_ < 0)) alpha *= .8;
    alpha;
  }

  private def computeAffineScalingDir(A: DenseMatrix[Double], b: DenseVectorCol[Double], c: DenseVectorCol[Double], x: DenseVectorCol[Double], s: DenseVectorCol[Double], z: DenseVectorCol[Double]) = {
    val XiZ = diag(z :/ s);

    val AtXiZ = (A.t * XiZ).asInstanceOf[DenseMatrix[Double]]

    val rx = A * x + s - b;
    val rz = A.t * z + c

    val dx = (AtXiZ * A) \ (A.t * z - rz - AtXiZ * rx)
    val ds = -rx - A * dx
    val dz = -z - XiZ * ds
    (dz,dx,ds);
  }


  private def computeCenteringCorrectorDir(A: DenseMatrix[Double], b: DenseVectorCol[Double], c: DenseVectorCol[Double], x: DenseVectorCol[Double], s: DenseVectorCol[Double], z: DenseVectorCol[Double], dsaff: DenseVectorCol[Double], dzaff: DenseVectorCol[Double], sigma: Double) = {
    val n = A.numCols;
    val m = A.numRows;
    import DenseMatrix._;
    val mat = vertcat(horzcat[Double](zeros(m,m), A, eye(m)),
                      horzcat[Double](A.t, zeros(n,n + m)),
                      horzcat[Double](diag(s),zeros(m,n),diag(z)));

    val r = DenseVector.zeros[Double](m + n + m);
    r((m+n) until (m+n+m)) -= (dsaff :* dzaff - sigma/m * (s dot z) );
    val sol = mat \ r;
    (r(0 until m),r(m until (n+m)),r( (n+m) until (n+m+m)));
  }




}

