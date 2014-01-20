package breeze.linalg

import breeze.generic.UFunc
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}

/**
 * Computes the SVD of a m by n matrix
 * Returns an m*m matrix U, a vector of singular values, and a n*n matrix V'
 */
object svd extends UFunc {
  implicit object Svd_DM_Impl extends Impl[DenseMatrix[Double], (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])] {
    def apply(mat: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {
      requireNonEmptyMatrix(mat)

      val m = mat.rows
      val n = mat.cols
      val S = DenseVector.zeros[Double](m min n)
      val U = DenseMatrix.zeros[Double](m,m)
      val Vt = DenseMatrix.zeros[Double](n,n)
      val iwork = new Array[Int](8 * (m min n) )
      val workSize = ( 3
        * scala.math.min(m, n)
        * scala.math.min(m, n)
        + scala.math.max(scala.math.max(m, n), 4 * scala.math.min(m, n)
        * scala.math.min(m, n) + 4 * scala.math.min(m, n))
        )
      val work = new Array[Double](workSize)
      val info = new intW(0)
      val cm = copy(mat)

      lapack.dgesdd(
        "A", m, n,
        cm.data, scala.math.max(1,m),
        S.data, U.data, scala.math.max(1,m),
        Vt.data, scala.math.max(1,n),
        work,work.length,iwork, info)

      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      (U,S,Vt)
    }
  }
}
