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
  implicit object impl_DM_Double extends Impl[DenseMatrix[Double], (DenseMatrix[Double], DenseMatrix[Double])] {
    def apply(v: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {
      doQr(v, false)
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

    implicit def canJustRIfWeCanQR[T,Q,R](implicit qrImpl: qr.Impl[T, (Q, R)]):Impl[T, R] = {
      new Impl[T, R] {
        def apply(v: T): R = qrImpl(v)._2
      }

    }
  }

  /**
   * QR that just returns Q.
   */
  object justQ extends UFunc {

    implicit def canJustRIfWeCanQR[T,Q,R](implicit qrImpl: qr.Impl[T, (Q, R)]):Impl[T, Q] = {
      new Impl[T, Q] {
        def apply(v: T): Q = qrImpl(v)._1
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
}
