package breeze.linalg

import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
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
    def apply( X: DenseMatrix[Double]): (DenseMatrix[Double], Array[Int]) = {

      val M    = X.rows
      val N    = X.cols
      val Y    = X.copy
      val ipiv = Array.ofDim[Int](scala.math.min(M,N))
      val info = new intW(0)
      lapack.dgetrf(
        M /* rows */, N /* cols */,
        Y.data, scala.math.max(1,M) /* LDA */,
        ipiv /* pivot indices */,
        info
      )
      // A value of info.`val` < 0 would tell us that the i-th argument
      // of the call to dsyev was erroneous (where i == |info.`val`|).
      assert(info.`val` >= 0)

      (Y, ipiv)
    }

  }

  implicit def LU_DM_Cast_Impl_Double[T](implicit cast: T=>Double):Impl[DenseMatrix[T], (DenseMatrix[Double], Array[Int])] = {
    new Impl[DenseMatrix[T], (DenseMatrix[Double], Array[Int])] {
      def apply(v: DenseMatrix[T]): (DenseMatrix[Double], Array[Int]) = {
        import DenseMatrix.canMapValues
        LU_DM_Impl_Double(v.mapValues(cast))
      }
    }
  }

  implicit object LU_DM_Impl_Float extends Impl[DenseMatrix[Float], (DenseMatrix[Float], Array[Int])] {
    def apply(X: DenseMatrix[Float]): (DenseMatrix[Float], Array[Int]) = {
      val M    = X.rows
      val N    = X.cols
      val Y    = X.copy
      val ipiv = Array.ofDim[Int](scala.math.min(M,N))
      val info = new intW(0)
      lapack.sgetrf(
        M /* rows */, N /* cols */,
        Y.data, scala.math.max(1,M) /* LDA */,
        ipiv /* pivot indices */,
        info
      )
      // A value of info.`val` < 0 would tell us that the i-th argument
      // of the call to dsyev was erroneous (where i == |info.`val`|).
      assert(info.`val` >= 0)

      (Y, ipiv)
    }
  }

  // Note: Not sure if this method could be faster or not, but after hours of searching how to 
  // convert from the vector to the matrix, this is what I came up with.
  /**
    * Creates the permutation matrix from a pivot vector (result from LAPACK)
    * @param ipiv - The pivot vector returned from LAPACK
    * @param rows - The number of rows in the original matrix
    * @param cols - The number of columns in the original matrix
    * @returns The permutation matrix, P from the LU decomposition of the form (P * L * U) 
     */
  private def createPermutationMatrix(ipiv: Array[Int], rows:Int, cols: Int): DenseMatrix[Double] = {
    val ipiv_dim: Int = min(rows, cols).toInt
    var parr: Array[Int] = new Array(rows)

    // Create perm vector
    for (x <- 0 to rows-1) {
      parr(x) = x + 1
    }
    for(i <- 0 to ipiv_dim-1) {
      val i2: Int = ipiv(i) - 1
      var tmp: Int = parr(i)
      parr(i) = parr(i2)
      parr(i2) = tmp
    }

    // Create permutation matrix
    var pm: DenseMatrix[Double] = DenseMatrix.eye[Double](rows)
    for( r1 <- ipiv_dim - 1 to 0 by -1) {
      val r2: Int = ipiv(r1) - 1
      if (r1 != r2) {
        for (col <- 0 to rows - 1) {
          // Swap pm(r1, col) with pm(row2, col)
          val tmp: Double = pm(r1, col)
          pm(r1, col) = pm(r2, col)
          pm(r2, col) = tmp
        }
      }
    }
    pm
  }

  /**
    * Decomposes the output of the LAPACK LU decomposition
    * @param X - The packed upper/lower matrices from LAPACK
    * @param ipiv - The pivot vector from LAPACK
    * @return (DenseMatrix, DenseMatrix, DenseMatrix) - Three matrices P, L, and U which give the original matrix when multiplied together
    * i.e X, ipiv = LU(A), P, L, U = decompose(X, ipiv), P * L * U = A
  */
  def decompose(X: DenseMatrix[Double], ipiv: Array[Int]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]) = {
    val P: DenseMatrix[Double] = createPermutationMatrix(ipiv, X.row.toInt, X.cols.toInt)
    val L: DenseMatrix[Double] = lowerTriangular(X)
    val U: DenseMatrix[Double] = upperTriangular(X)
    (P, L, U)
  }

  /** 
    * Returns the lower triangular matrix from X.
    * Differs from Breeze's currently implemented method in that it works on non-square matrices
   */
  private def lowerTriangular(X: DenseMatrix[Double]): DenseMatrix[Double] = {
    DenseMatrix.tabulate(X.rows, X.cols)( (i, j) =>
      if (j == i) {
        1
      }else if (j < i) {
        X(i, j)
      } else {
        implicitly[Semiring[Double]].zero
      }
    )
  }

  /** 
    * Returns the upper triangular matrix from X.
    * Differs from Breeze's currently implemented method in that it works on non-square matrices
   */
  private def upperTriangular(X: DenseMatrix[Double]): DenseMatrix[Double] = {
    DenseMatrix.tabulate(X.rows, X.cols)( (i, j) =>
      if (j == i) {
        X(i, j) // Keep values on the diagonal (unlike in lower triangular)
      } else if (j > i) {
        X(i, j)
      } else {
        implicitly[Semiring[Double]].zero
      }
    )
  }

}
