package breeze.linalg

import breeze.generic.UFunc
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}


sealed trait QRMode
case object CompleteQR extends QRMode  // Q and R have dimensions (m, m), (m, n)
case object ReducedQR extends QRMode   // Q and R have dimensions (m, k), (k, n) with k = min(m, n)


/**
 * QR Factorization
 *
 * Previous versions of Breeze had qr(m, skipQ), where we could
 * skip the computation in making Q if we didn't want it. That is now supplanted by qr.justR(m)
 *
 * Supports complete and reduced mode of factorization of matrix A with dimensions (m, n).
 * If mode is complete matrices Q and R have dimensions (m, m), (m, n).
 * If mode is reduced matrices Q and R have dimensions (m, k), (k, n) with k = min(m, n).
 *
 * Complete QR factorization can be called by qr(A).
 *
 * Reduced QR factorization can be called by qr.reduced(A). If computation of Q is unnecessary, it
 * can be skipped by qr.reduced.justR(A)
 *
 * @return (Q, R)
 *         Q - A matrix with orthonormal columns
 *         R - The upper-triangular matrix
 *
 */
object qr extends UFunc {

  case class QR[M](q: M, r: M)

  type DenseQR = QR[DenseMatrix[Double]]

  implicit object impl_DM_Double extends Impl[DenseMatrix[Double],DenseQR] {
    def apply(v: DenseMatrix[Double]): DenseQR = {
      val (q, r) = doQr(v, skipQ = false)(mode = CompleteQR)
      QR(q, r)
    }
  }

  /**
   * QR that just returns R.
   */
  object justR extends UFunc {
    implicit object impl_DM_Double extends Impl[DenseMatrix[Double], DenseMatrix[Double]] {
      def apply(v: DenseMatrix[Double]): DenseMatrix[Double] = {
        doQr(v, skipQ = true)(mode = CompleteQR)._2
      }
    }

    implicit def canJustQIfWeCanQR[T,M](implicit qrImpl: qr.Impl[T, QR[M]]):Impl[T, M] = {
      new Impl[T, M] {
        def apply(v: T): M = qrImpl(v).r
      }

    }
  }

  /**
   * QR that just returns Q.
   */
  object justQ extends UFunc {

    implicit def canJustQIfWeCanQR[T,M](implicit qrImpl: qr.Impl[T, QR[M]]):Impl[T, M] = {
      new Impl[T, M] {
        def apply(v: T): M = qrImpl(v).q
      }

    }
  }


  /**
   * QR Factorization that returns Q and R with dimensions (m, k), (k, n) where k = min(m, n)
   */
  object reduced extends UFunc {

    implicit object impl_reduced_DM_Double extends Impl[DenseMatrix[Double],DenseQR] {
      def apply(v: DenseMatrix[Double]): DenseQR = {
        val (q, r) = doQr(v, skipQ = false)(mode = ReducedQR)
        QR(q, r)
      }
    }

    /**
     * QR that just returns R with reduced size.
     */
    object justR extends UFunc {
      implicit object impl_reduced_DM_Double extends Impl[DenseMatrix[Double], DenseMatrix[Double]] {
        def apply(v: DenseMatrix[Double]): DenseMatrix[Double] = {
          doQr(v, skipQ = true)(mode = ReducedQR)._2
        }
      }

      implicit def canJustQIfWeCanQR[T,M](implicit qrImpl: qr.reduced.Impl[T, QR[M]]):Impl[T, M] = {
        new Impl[T, M] {
          def apply(v: T): M = qrImpl(v).r
        }
      }
    }

    /**
     * QR that just returns Q with reduced size.
     */
    object justQ extends UFunc {
      implicit def canJustQIfWeCanQR[T, M](implicit qrImpl: qr.reduced.Impl[T, QR[M]]): Impl[T, M] = {
        new Impl[T, M] {
          def apply(v: T): M = qrImpl(v).q
        }
      }
    }
  }

  private def doQr(M: DenseMatrix[Double], skipQ: Boolean)
                  (mode: QRMode): (DenseMatrix[Double], DenseMatrix[Double]) = {

    val A = M.copy

    val m = A.rows
    val n = A.cols

    val mn = scala.math.min(m,n)
    val tau = new Array[Double](mn)

    // Calculate optimal size of work data 'work'
    val work = new Array[Double](1)
    val info = new intW(0)
    lapack.dgeqrf(m, n, A.data, m, tau, work, -1, info)

    // do QR
    val lwork = if (info.`val` != 0) n else work(0).toInt
    val workspace = new Array[Double](lwork)

    lapack.dgeqrf(m, n, A.data, m, tau, workspace, lwork, info)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    // Handle mode that don't return Q
    if (skipQ) (null, upperTriangular(A(0 until mn, ::)))
    else {
      val Q = if (mode == CompleteQR && m > n) DenseMatrix.zeros[Double](m, m)
              else DenseMatrix.zeros[Double](m, n)

      val mc = if (mode == CompleteQR && m > n) m else mn

      Q(::, 0 until n) := A

      // Determine optimal size
      lapack.dorgqr(m, mc, mn, Q.data, m, tau, work, -1, info)
      // Compute Q
      val lwork1 = if (info.`val` != 0) n else work(0).toInt
      val workspace1 = new Array[Double](lwork1)
      lapack.dorgqr(m, mc, mn, Q.data, m, tau, workspace1, lwork1, info)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      // Upper triangle
      for {
        i <- 0 until mc
        j <- 0 until A.cols
        if j < i
      } A(i, j) = 0.0

      (Q(::, 0 until mc), A(0 until mc, ::))
    }
  }
}


/**
 * QR Factorization with pivoting
 *
 * input: A m x n matrix
 * output: (Q,R,P,pvt) where AP = QR
 *   Q: m x m
 *   R: m x n
 *   P: n x n : permutation matrix (P(pvt(i),i) = 1)
 *   pvt : pivot indices
 */
object qrp extends UFunc {
  case class QRP[M, PivotMatrix](q: M, r: M, pivotMatrix: PivotMatrix, pivotIndices: Array[Int])

  type DenseQRP = QRP[DenseMatrix[Double], DenseMatrix[Int]]

  implicit object impl_DM_Double extends Impl[DenseMatrix[Double], DenseQRP] {
    def apply(A: DenseMatrix[Double]): DenseQRP = {
      val m = A.rows
      val n = A.cols

      //Get optimal workspace size
      // we do this by sending -1 as lwork to the lapack function
      val scratch, work = new Array[Double](1)
      var info = new intW(0)
      lapack.dgeqrf(m, n, scratch, m, scratch, work, -1, info)
      val lwork1 = if(info.`val` != 0) n else work(0).toInt
      lapack.dorgqr(m, m, scala.math.min(m,n), scratch, m, scratch, work, -1, info)
      val lwork2 = if(info.`val` != 0) n else work(0).toInt
      //allocate workspace mem. as max of lwork1 and lwork3
      val workspace = new Array[Double](scala.math.max(lwork1, lwork2))

      //Perform the QR factorization with dgep3
      val maxd = scala.math.max(m,n)
      val AFact = DenseMatrix.zeros[Double](m,maxd)
      val pvt = new Array[Int](n)
      val tau = new Array[Double](scala.math.min(m,n))
      for(r <- 0 until m; c <- 0 until n) AFact(r,c) = A(r,c)
      lapack.dgeqp3(m, n, AFact.data, m, pvt, tau, workspace, workspace.length, info)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      //Get R
      val R = DenseMatrix.zeros[Double](m,n)

      for(c <- 0 until maxd if(c < n); r <- 0 until m if(r <= c))
        R(r,c) = AFact(r,c)

      //Get Q from the matrix returned by dgep3
      val Q = DenseMatrix.zeros[Double](m,m)
      lapack.dorgqr(m, m, scala.math.min(m,n), AFact.data, m, tau, workspace, workspace.length, info)
      for(r <- 0 until m; c <- 0 until maxd if(c < m))
        Q(r,c) = AFact(r,c)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      //Get P
      import NumericOps.Arrays._
      pvt -= 1
      val P = DenseMatrix.zeros[Int](n,n)
      for(i <- 0 until n)
        P(pvt(i), i) = 1

      QRP(Q,R,P,pvt)
    }
  }
}
