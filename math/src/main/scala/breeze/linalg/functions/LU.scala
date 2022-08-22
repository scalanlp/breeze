package breeze.linalg

import org.netlib.util.intW
import dev.ludovic.netlib.lapack.LAPACK.{ getInstance => lapack }
import breeze.generic.UFunc
import breeze.math.Semiring
import breeze.storage.Zero
import breeze.util.ReflectionUtil

import scala.reflect.ClassTag
/**
 * Computes the LU factorization of the given real M-by-N matrix X such that
 * X = P * L * U where P is a permutation matrix (row exchanges).
 *
 *
 */
object LU extends UFunc {

  case class LU[M](P: M, L: M, U: M)

  type DenseLU[T] = LU[DenseMatrix[T]]

  implicit def fromPrimitiveDecomposition[T, U](implicit prim: primitive.Impl[DenseMatrix[T], (DenseMatrix[U], Array[Int])], ct: ClassTag[U], semi: Semiring[U]): Impl[DenseMatrix[T], DenseLU[U]] = {
    new Impl[DenseMatrix[T], DenseLU[U]] {
      override def apply(v: DenseMatrix[T]): DenseLU[U] = {
        val (m, p) = prim(v)
        decompose(m, p)
      }
    }
  }

  implicit def fromPrimitiveDecompositionSimple[T](implicit prim: primitive.Impl[DenseMatrix[T], (DenseMatrix[T], Array[Int])], ct: ClassTag[T], semi: Semiring[T]): Impl[DenseMatrix[T], DenseLU[T]] = {
    new Impl[DenseMatrix[T], DenseLU[T]] {
      override def apply(v: DenseMatrix[T]): DenseLU[T] = {
        val (m, p) = prim(v)
        decompose(m, p)
      }
    }
  }

  /**
    * Returns the raw lapack result, which is more memory efficient, but harder to work with.
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
    **/
  object primitive extends UFunc {

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
//          import DenseMatrix.canMapValues
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

  /**
    * Creates the permutation matrix from the IPIV (pivot) vector result from LAPACK
    * Traditionally, the decomposition is represented as P * X = L * U
    * Thus for a matrix with R rows and C columns, the matrix P should be of size
    * RxR in order to preserve the original dimension of the matrix X.
    *
    * @param ipiv - The pivot vector returned from LAPACK
    * @param rows - The number of rows in the X matrix
    * @param cols - The number of columns in the X matrix
    * @return The permutation matrix, P from the LU decomposition of the form (P * X = L * U)
    *         size RxR
     */
  def createPermutationMatrix[T: ClassTag: Semiring](ipiv: Array[Int], rows:Int, cols: Int): DenseMatrix[T] = {
    val indices: Array[Int] = new Array[Int](rows)
    for(i <- 0 until rows) {
      indices(i) = i
    }

    for(i <- 0 until ipiv.length) {
      val j = ipiv(i) - 1
      val t = indices(i)
      indices(i) = indices(j)
      indices(j) = t
    }

    val pm: DenseMatrix[T] = DenseMatrix.zeros[T](rows, rows)
    for(i <- 0 until rows) {
      pm(indices(i), i) = implicitly[Semiring[T]].one
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
  def decompose[T: Semiring: ClassTag](X: DenseMatrix[T], ipiv: Array[Int]): DenseLU[T] = {
    val P: DenseMatrix[T] = createPermutationMatrix(ipiv, X.rows, X.cols)
    val L: DenseMatrix[T] = lowerTriangular(X)
    val U: DenseMatrix[T] = upperTriangular(X)
    LU(P, L, U)
  }

  /** 
    * Returns the lower triangular matrix from X.
    * Differs from Breeze's currently implemented method in that it works on non-square matrices
    * Returns 1 on the diagonal
   */
  private def lowerTriangular[T: Semiring: ClassTag](X: DenseMatrix[T]): DenseMatrix[T] = {
    DenseMatrix.tabulate(X.rows, X.cols)( (i, j) =>
      if (j == i) {
        implicitly[Semiring[T]].one
      } else if (j < i) {
        X(i, j)
      } else {
        implicitly[Semiring[T]].zero
      }
    )
  }

  /** 
    * Returns the upper triangular matrix from X.
    * Differs from Breeze's currently implemented method in that it works on non-square matrices
    * Returns current values on the diagonal.
   */
  private def upperTriangular[T: Zero: ClassTag](X: DenseMatrix[T]): DenseMatrix[T] = {
    DenseMatrix.tabulate(X.rows, X.cols)( (i, j) =>
      if (j == i) {
        X(i, j) // Keep values on the diagonal (unlike in lower triangular)
      } else if (j > i) {
        X(i, j)
      } else {
        implicitly[Zero[T]].zero
      }
    )
  }

}
