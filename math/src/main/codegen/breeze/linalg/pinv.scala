package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.operators.{OpMulMatrix, OpSolveMatrixBy}
import breeze.linalg.support.CanTranspose
import breeze.macros.expand

/**
 * Computes the Moore-Penrose pseudo inverse of the given real matrix X.
 *
 * The pseudo inverse is nothing but the least-squares solution to AX=B,
 * hence:
 * d/dX 1/2 (AX-B)^2 = A^T (AX-B)
 * Solving A^T (AX-B) = 0 for X yields
 * A^T AX = A^T B
 * =>      X = (A^T A)^(-1) A^T B
 *
 * @param v: Matrix to be pseudo-inverted
 * @param rcond: Cutoff for small singular values. Singular values less than or equal to rcond * largest_singular_value
 *               are set to zero. Default: 1e-15. To deactivate this option, set rcond to zero.
 */
object pinv extends UFunc with pinvLowPrio {

  private val DEFAULT_RCOND = 1e-15f

  @expand
  @expand.valify
  implicit def pinvFromSVD[@expand.args(Float, Double) T]: Impl[DenseMatrix[T], DenseMatrix[T]] = {
    new Impl[DenseMatrix[T], DenseMatrix[T]] {
      // No rcond passed as parameter, use default value
      override def apply(v: DenseMatrix[T]): DenseMatrix[T] = {
          val rcond: T = DEFAULT_RCOND
          pinv(v, rcond)
      }
    }
  }


  @expand
  @expand.valify
  implicit def pinvFromSVDRcond[@expand.args(Float, Double) T]: Impl2[DenseMatrix[T], T, DenseMatrix[T]] = {
    new Impl2[DenseMatrix[T], T, DenseMatrix[T]] {
      // http://en.wikipedia.org/wiki/Singular_value_decomposition#Applications_of_the_SVD
      override def apply(v: DenseMatrix[T], rcond: T): DenseMatrix[T] = {
        require(rcond >= 0, "rcond must be non-negative")

        val svd.SVD(s, svs, d) = svd(v)
        val cutoff = max(svs) * rcond
        val vi = svs.map { v =>
          if (v <= cutoff) 0 else 1 / v
        }

        val svDiag = DenseMatrix.tabulate[T](s.cols, d.rows) { (i, j) =>
          if (i == j && i < math.min(s.cols, d.rows)) vi(i)
          else 0
        }
        val res = s * svDiag * d
        res.t
      }
    }
  }

}

trait pinvLowPrio { self: pinv.type =>

  /**
   * pinv for anything that can be transposed, multiplied with that transposed, and then solved.
   * This signature looks intense, but take it one step at a time.
   * @param numericT : Do I support operators
   * @param trans : Can I be transposed?
   * @param numericTrans : Does my transpose support operators
   * @param mul : Can I multiply T and TransT?
   * @param numericMulRes : Does the result of that multiplication support operators?
   * @param solve : Can I solve the system of equations MulRes * x = TransT
   * @tparam T the type of matrix
   * @tparam TransT the transpose of that matrix
   * @tparam MulRes the result of TransT * T
   * @tparam Result the result of MulRes \ TransT
   * @return
   */
  implicit def implFromTransposeAndSolve[T, TransT, MulRes, Result](
      implicit numericT: T => NumericOps[T],
      trans: CanTranspose[T, TransT],
      numericTrans: TransT => NumericOps[TransT],
      mul: OpMulMatrix.Impl2[TransT, T, MulRes],
      numericMulRes: MulRes => NumericOps[MulRes],
      solve: OpSolveMatrixBy.Impl2[MulRes, TransT, Result]): Impl[T, Result] = {
    new Impl[T, Result] {
      def apply(X: T): Result = {
        (X.t * X) \ X.t
      }
    }
  }

}
