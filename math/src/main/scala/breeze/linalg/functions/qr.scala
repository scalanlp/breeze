package breeze.linalg

import breeze.generic.UFunc
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}

/**
 * QR Factorization
 *
 * Previous versions of Breeze had qr(m, skipQ), where we could
 * skip the computation in making q if we didn't want it. That is now supplanted by qr.justR(m)
 *
 * @return (Q,R) Q: m x m R: m x n
 *
 */
object qr extends UFunc {

  case class QR[M](q: M, r: M)

  type DenseQR = QR[DenseMatrix[Double]]

  implicit object impl_DM_Double extends Impl[DenseMatrix[Double],DenseQR] {
    def apply(v: DenseMatrix[Double]): DenseQR = {
      val (q, r) = doQr(v, false)
      QR(q, r)
    }
  }

  /**
   * QR that just returns R.
   */
  object justR extends UFunc {
    implicit object impl_DM_Double extends Impl[DenseMatrix[Double], DenseMatrix[Double]] {
      def apply(v: DenseMatrix[Double]): DenseMatrix[Double] = {
        doQr(v, true)._2
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
   * QR Factorization
   *
   * @param A m x n matrix
   * @param skipQ (optional) if true, don't reconstruct orthogonal matrix Q (instead returns (null,R))
   * @return (Q,R) Q: m x m R: m x n
   */
  private def doQr(A: DenseMatrix[Double], skipQ : Boolean = false): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val m = A.rows
    val n = A.cols

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    // can't pass in null arrays on linux to new lapack, so passing in a length 1 called scratch
    val scratch, work = new Array[Double](1)
    val info = new intW(0)
    lapack.dgeqrf(m, n, scratch, m, scratch, work, -1, info)
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, scala.math.min(m,n), scratch, m, scratch, work, -1, info)
    val lwork2 = if(info.`val` != 0) n else work(0).toInt
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](scala.math.max(lwork1, lwork2))

    //Perform the QR factorization with dgeqrf
    val maxd = scala.math.max(m,n)
    val mind = scala.math.min(m,n)
    val tau = new Array[Double](mind)
    val outputMat = DenseMatrix.zeros[Double](m,maxd)
    for(r <- 0 until m; c <- 0 until n)
      outputMat(r,c) = A(r,c)
    lapack.dgeqrf(m, n, outputMat.data, m, tau, workspace, workspace.length, info)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get R
    val R = DenseMatrix.zeros[Double](m,n)
    for(c <- 0 until maxd if(c < n); r <- 0 until m if(r <= c))
      R(r,c) = outputMat(r,c)

    //unless the skipq flag is set
    if(!skipQ){
      //Get Q from the matrix returned by dgep3
      val Q = DenseMatrix.zeros[Double](m,m)
      lapack.dorgqr(m, m, scala.math.min(m,n), outputMat.data, m, tau, workspace, workspace.length, info)
      for(r <- 0 until m; c <- 0 until maxd if(c < m))
        Q(r,c) = outputMat(r,c)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()
      (Q,R)
    }
    //skip Q and just return R
    else (null,R)
  }


  /**
   * Factor the matrix A as QR, where Q is orthonormal and R is upper-triangular.
   * Q and R have reduced size.
   */
  object reduced extends UFunc {

    implicit object impl_reduced_DM_Double extends Impl[DenseMatrix[Double],DenseQR] {
      def apply(v: DenseMatrix[Double]): DenseQR = {
        val (q, r) = doQrReduced(v, false)
        QR(q, r)
      }
    }


    /**
     * QR that just returns R.
     * R has reduced size.
     */
    object justR extends UFunc {
      implicit object impl_reduced_DM_Double extends Impl[DenseMatrix[Double], DenseMatrix[Double]] {
        def apply(v: DenseMatrix[Double]): DenseMatrix[Double] = {
          doQrReduced(v, true)._2
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
     * Q has reduced size.
     */
    object justQ extends UFunc {
      implicit def canJustQIfWeCanQR[T, M](implicit qrImpl: qr.Impl[T, QR[M]]): Impl[T, M] = {
        new Impl[T, M] {
          def apply(v: T): M = qrImpl(v).q
        }
      }
    }

  }

  /**
   * Factor the matrix A as QR, where Q is orthonormal and R is upper-triangular.
   * Q and R have reduced size.
   *
   * @param A m x n matrix
   * @param skipQ (optional) if true, don't reconstruct orthogonal matrix Q (instead returns (null,R))
   * @return (Q,R) Q: m x k R: k x n K = min(m, n)
   */
  private def doQrReduced(A: DenseMatrix[Double], skipQ : Boolean = false): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val m = A.rows
    val n = A.cols

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    // can't pass in null arrays on linux to new lapack, so passing in a length 1 called scratch
    val scratch, work = new Array[Double](1)
    val info = new intW(0)
    lapack.dgeqrf(m, n, scratch, m, scratch, work, -1, info)
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, scala.math.min(m,n), scratch, m, scratch, work, -1, info)
    val lwork2 = if(info.`val` != 0) n else work(0).toInt
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](scala.math.max(lwork1, lwork2))

    //Perform the QR factorization with dgeqrf
    val maxd = scala.math.max(m,n)
    val mind = scala.math.min(m,n)
    val tau = new Array[Double](mind)
    val outputMat = DenseMatrix.zeros[Double](m,maxd)
    for(r <- 0 until m; c <- 0 until n)
      outputMat(r,c) = A(r,c)
    lapack.dgeqrf(m, n, outputMat.data, m, tau, workspace, workspace.length, info)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get R
    val R = DenseMatrix.zeros[Double](mind,n)
    for(c <- 0 until maxd if(c < n); r <- 0 until m if(r <= c))
      R(r,c) = outputMat(r,c)

    //unless the skipq flag is set
    if(!skipQ){
      //Get Q from the matrix returned by dgep3
      val Q = DenseMatrix.zeros[Double](m,mind)
      lapack.dorgqr(m, mind, mind, outputMat.data, m, tau, workspace, workspace.length, info)
      for(r <- 0 until m; c <- 0 until mind)
        Q(r,c) = outputMat(r,c)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()
      (Q,R)
    }
    //skip Q and just return R
    else (null,R)
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
