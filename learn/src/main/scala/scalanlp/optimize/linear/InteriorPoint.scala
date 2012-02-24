package scalanlp.optimize
package linear


import scalala.library.Library._;
import scalala.library.LinearAlgebra._
import scalala.tensor.dense.{DenseVectorCol, DenseVector, DenseMatrix}
import scalala.tensor.{MatrixSingularException, Vector, ::}
;

/**
 * InteriorPoint solver for LPs.
 * @author dlwh
 */
object InteriorPoint {
  val TOLERANCE = 1E-18
  /**
   *
   * Solves the LP:
   *
   * minimize_x c * x
   * subject to A * x <= b
   *
   * with initial feasible point x0
   *
   * @see http://www.doc.ic.ac.uk/~br/berc/pdiplin.pdf
   * @see http://www.ee.ucla.edu/ee236a/lectures/mpc.pdf
   */

  def minimize(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x0: DenseVector[Double], tol: Double = TOLERANCE): DenseVectorCol[Double] = {
    val m = A.numRows;
    val n = A.numCols;
    val x = DenseVector.zeros[Double](n)
    x += x0;
    if( (A * x0.asCol - b).values.exists(_ > 0)) {
      x := phase1(A,b,c,x0)
    }
//    assert((A * x0.asCol - b).valuesIterator.forall(_ <= 0));
    val s = DenseVector.zeros[Double](m);
    s += 1;
    val z = DenseVector.zeros[Double](m);
    z += 1;
//    assert((A.t * z + s - c).valuesIterator.forall(_.abs <= 1E-4));
    var converged = false;
    var lastGap = Double.PositiveInfinity
    while(!converged) {

      try {

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
      converged = gap.abs < tol
      if(gap > lastGap) {
        x -= dx * (.99 * scaleXF)
      }
      lastGap = gap
      } catch {
        case m: MatrixSingularException => converged = true
      }
//      assert(gap > -1E-3,gap);
    }

    x
  }

  // find a feasible point
  private def phase1(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x0: DenseVector[Double]) = {
    val s = (A * x0.asCol - b).values.max + 1E-7
    val newA = DenseMatrix.zeros[Double](A.numRows+1,A.numCols+1)
    newA(0 until  A.numRows,0 until A.numCols) := A
    newA(0 until A.numRows+1,A.numCols) := -1.0

    val newB = DenseVector.tabulate(b.size + 1)(i => if(i < b.size) b(i) else 0)
    val newC = DenseVector.zeros[Double](c.size + 1)
    newC(c.size) = 1
    val newX = DenseVector.tabulate(x0.size + 1)(i => if(i < x0.size) x0(i) else s)
    if( (newA * newX - newB).values.exists(_ > 0)) {
      throw new RuntimeException("Problem seems to be infeasible!")
    }
    val r = minimize(newA,newB,newC,newX)
    if(r(x0.size) > 1E-8)
      println("Problem appears to be infeasible: " + r(x0.size))
    r(0 until x0.size)
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

    val mat = AtXiZ * A
//    mat += diag(DenseVector.fill(mat.numRows)(1E-20))

    val dx = (mat) \ (A.t * z - rz - AtXiZ * rx)
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
                      horzcat[Double](diag(s),zeros(m,n),diag(z)))

    mat += diag(DenseVector.fill(mat.numRows)(1E-20))

    val r = DenseVector.zeros[Double](m + n + m);
    r((m+n) until (m+n+m)) -= (dsaff :* dzaff - sigma/m * (s dot z) );
    val sol = mat \ r;
    (r(0 until m),r(m until (n+m)),r( (n+m) until (n+m+m)));
  }




}

