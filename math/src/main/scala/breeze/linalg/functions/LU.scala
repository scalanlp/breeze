package breeze.linalg

import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}
import breeze.generic.UFunc

/**
 * Computes the LU factorization of the given real M-by-N matrix X such that
 * X = P * L * U where P is a permutation matrix (row exchanges).
 *
 * Upon completion, a tuple consisting of a matrix A and an integer array P.
 *
 * The upper triangular portion of A resembles U whereas the lower triangular portion of
 * A resembles L up to but not including the diagonal elements of L which are
 * all equal to 1.
 *
 * For 0 <= i < M, each element P(i) denotes whether row i of the matrix X
 * was exchanged with row P(i) - 1 during computation (the offset is caused by
 * the internal call to LAPACK).
 */
object LU extends UFunc {

  implicit object LU_DM_Impl_Double extends Impl[DenseMatrix[Double], (DenseMatrix[Double], Array[Int])] {
    def apply(X: DenseMatrix[Double]): (DenseMatrix[Double], Array[Int]) = {

      val M = X.rows
      val N = X.cols
      val Y = X.copy
      val ipiv = Array.ofDim[Int](scala.math.min(M, N))
      val info = new intW(0)
      lapack.dgetrf(
        M /* rows */,
        N /* cols */,
        Y.data,
        scala.math.max(1, M) /* LDA */,
        ipiv /* pivot indices */,
        info
      )
      // A value of info.`val` < 0 would tell us that the i-th argument
      // of the call to dsyev was erroneous (where i == |info.`val`|).
      assert(info.`val` >= 0)

      (Y, ipiv)
    }

  }

  implicit def LU_DM_Cast_Impl_Double[T](
      implicit cast: T => Double): Impl[DenseMatrix[T], (DenseMatrix[Double], Array[Int])] = {
    new Impl[DenseMatrix[T], (DenseMatrix[Double], Array[Int])] {
      def apply(v: DenseMatrix[T]): (DenseMatrix[Double], Array[Int]) = {
        import DenseMatrix.canMapValues
        LU_DM_Impl_Double(v.mapValues(cast))
      }
    }
  }

  implicit object LU_DM_Impl_Float extends Impl[DenseMatrix[Float], (DenseMatrix[Float], Array[Int])] {
    def apply(X: DenseMatrix[Float]): (DenseMatrix[Float], Array[Int]) = {
      val M = X.rows
      val N = X.cols
      val Y = X.copy
      val ipiv = Array.ofDim[Int](scala.math.min(M, N))
      val info = new intW(0)
      lapack.sgetrf(
        M /* rows */,
        N /* cols */,
        Y.data,
        scala.math.max(1, M) /* LDA */,
        ipiv /* pivot indices */,
        info
      )
      // A value of info.`val` < 0 would tell us that the i-th argument
      // of the call to dsyev was erroneous (where i == |info.`val`|).
      assert(info.`val` >= 0)

      (Y, ipiv)
    }
  }

}
