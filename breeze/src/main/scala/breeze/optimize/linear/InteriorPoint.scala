package breeze.optimize
package linear


import breeze.linalg._

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

  def minimize(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x0: DenseVector[Double], tol: Double = TOLERANCE): DenseVector[Double] = {
    val m = A.rows
    val n = A.cols
    val x = DenseVector.zeros[Double](n)
    x += x0
    if( (A * x0 - b).values.exists(_ > 0)) {
      x := phase1(A,b,c,x0)
    }
//    assert((A * x0 - b).valuesIterator.forall(_ <= 0))
    val s = DenseVector.ones[Double](m)
    val z = DenseVector.ones[Double](m)
//    assert((A.t * z + s - c).valuesIterator.forall(_.abs <= 1E-4))
    var converged = false
    var lastGap = Double.PositiveInfinity
    while(!converged) {

      try {

      val (zAff,xAff,sAff) = computeAffineScalingDir(A,b,c,x,s,z)
      val scaleX = lineSearch(s,sAff)
      val scaleZ = lineSearch(z,zAff)
      val sigma = math.pow( (s + sAff * scaleX).dot(z + zAff * scaleZ)/(s dot z) ,3)
      val (zCC,xCC,sCC) = computeCenteringCorrectorDir(A,b,c,x,s,z,sAff,zAff,sigma)

      val dz = zAff += zCC
      val dx = xAff += xCC
      val ds = sAff += sCC

      val scaleXF = lineSearch(s,ds)
      val scaleZF = lineSearch(z,dz)

      axpy((.99 * scaleXF), dx, x)
      axpy((.99 * scaleXF), ds, s)
      axpy((.99 * scaleZF), dz, z)

      val gap = (c dot x) + (b dot z)
      converged = gap.abs < tol
      if(gap > lastGap) {
        axpy(-(.99 * scaleXF), dx, x)
      }
      lastGap = gap
      } catch {
        case m: MatrixSingularException => converged = true
      }
//      assert(gap > -1E-3,gap)
    }

    x
  }

  // find a feasible point
  private def phase1(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x0: DenseVector[Double]) = {
    val s = max(A * x0 - b) + 1E-7
    val newA = DenseMatrix.zeros[Double](A.rows+1,A.cols+1)
    newA(0 until  A.rows,0 until A.cols) := A
    newA(0 until A.rows+1,A.cols) := -1.0

    val newB = DenseVector.tabulate(b.size + 1)(i => if(i < b.size) b(i) else 0)
    val newC = DenseVector.zeros[Double](c.size + 1)
    newC(c.size) = 1
    val newX = DenseVector.tabulate(x0.size + 1)(i => if(i < x0.size) x0(i) else s)
    if ( any((newA * newX - newB) :> 0.0) ) {
      throw new RuntimeException("Problem seems to be infeasible!")
    }
    val r = minimize(newA,newB,newC,newX)
    if(r(x0.size) > 1E-8)
      println("Problem appears to be infeasible: " + r(x0.size))
    r.slice(0, x0.size)
  }

  private def lineSearch(x: DenseVector[Double],dx: Vector[Double]):Double = {
    var alpha = 1.0
    while((x + dx*alpha).valuesIterator.exists(_ < 0)) alpha *= .8
    alpha
  }

  private def computeAffineScalingDir(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x: DenseVector[Double], s: DenseVector[Double], z: DenseVector[Double]): (DenseVector[Double], DenseVector[Double], DenseVector[Double]) = {
    val XiZ = diag(z :/ s)

    val AtXiZ = (A.t * XiZ).asInstanceOf[DenseMatrix[Double]]

    val rx = A * x + s - b
    val rz = A.t * z + c

    val mat = AtXiZ * A

    val dx = (mat) \ (A.t * z - rz - AtXiZ * rx)
    val ds = -rx - A * dx
    val dz = -z - XiZ * ds
    (dz,dx,ds)
  }


  private def computeCenteringCorrectorDir(A: DenseMatrix[Double], b: DenseVector[Double], c: DenseVector[Double], x: DenseVector[Double], s: DenseVector[Double], z: DenseVector[Double], dsaff: DenseVector[Double], dzaff: DenseVector[Double], sigma: Double) = {
    val n = A.cols
    val m = A.rows
    import DenseMatrix._
    val mat = vertcat[Double](horzcat(zeros[Double](m,m), A, eye[Double](m)),
                      horzcat(A.t, zeros[Double](n,n + m)),
                      horzcat(diag(s),zeros[Double](m,n),diag(z)))

    diag(mat) += 1E-20

    val r = DenseVector.zeros[Double](m + n + m)
    r.slice((m+n), (m+n+m)) -= (dsaff :* dzaff - sigma/m * (s dot z) )
    val sol = mat \ r
    (sol.slice(0, m),sol.slice(m, (n+m)),sol.slice((n+m), (n+m+m)))
  }




}

