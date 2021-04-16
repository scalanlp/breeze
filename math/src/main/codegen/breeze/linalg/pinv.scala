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
 */
object pinv extends UFunc with pinvLowPrio {

  @expand
  @expand.valify
  implicit def pinvFromSVD[@expand.args(Float, Double) T]: Impl[DenseMatrix[T], DenseMatrix[T]] = {
    new Impl[DenseMatrix[T], DenseMatrix[T]] {
      // http://en.wikipedia.org/wiki/Singular_value_decomposition#Applications_of_the_SVD
      override def apply(v: DenseMatrix[T]): DenseMatrix[T] = {
        val svd.SVD(s, svs, d) = svd(v)
        val vi = svs.map { v =>
          if (v == 0.0) 0.0f else 1 / v
        }

        val svDiag = DenseMatrix.tabulate[T](s.cols, d.rows) { (i, j) =>
          if (i == j && i < math.min(s.cols, d.rows)) vi(i)
          else 0.0f
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
