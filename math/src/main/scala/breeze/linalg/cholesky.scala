package breeze.linalg

import breeze.generic.UFunc
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}

/**
 * Computes the cholesky decomposition A of the given real symmetric
 * positive definite matrix X such that X = A A.t.
 *
 * TODO: For higher dimensionalities, the return value really should be a sparse matrix
 *       due to its inherent lower triangular nature.
 */
object cholesky extends UFunc {
  implicit object ImplCholesky_DM extends Impl[DenseMatrix[Double], DenseMatrix[Double]] {
    def apply(X: DenseMatrix[Double]): DenseMatrix[Double] = {
      requireNonEmptyMatrix(X)

      // As LAPACK doesn't check if the given matrix is in fact symmetric,
      // we have to do it here (or get rid of this time-waster as long as
      // the caller of this function is clearly aware that only the lower
      // triangular portion of the given matrix is used and there is no
      // check for symmetry).
      requireSymmetricMatrix(X)


      // Copy the lower triangular part of X. LAPACK will store the result in A
      val A: DenseMatrix[Double] = lowerTriangular(X)

      val N = X.rows
      val info = new intW(0)
      lapack.dpotrf(
        "L" /* lower triangular */,
        N /* number of rows */, A.data, scala.math.max(1, N) /* LDA */,
        info
      )
      // A value of info.`val` < 0 would tell us that the i-th argument
      // of the call to dpotrf was erroneous (where i == |info.`val`|).
      assert(info.`val` >= 0)

      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)

      A
    }

  }
}
