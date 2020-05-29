package breeze.linalg

import breeze.macros.expand
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}
import breeze.generic.UFunc
import breeze.linalg.operators.{OpSolveMatrixBy, OpMulMatrix}
import breeze.linalg.support.CanTranspose

/**
 * Computes the inverse of a given real matrix.
 * In general, you should avoid using this metho in combination with *.
 * Instead, wherever you might want to write inv(A) * B, you should write
 * A \ B.
 */
object inv extends UFunc {
  implicit def canInvUsingLU_Double[T](implicit
      luImpl: LU.primitive.Impl[T, (DenseMatrix[Double], Array[Int])]
  ): Impl[T, DenseMatrix[Double]] = {
    new Impl[T, DenseMatrix[Double]] {
      def apply(X: T): DenseMatrix[Double] = {
        // Should these type hints be necessary?
        val (m: DenseMatrix[Double], ipiv: Array[Int]) = LU.primitive(X)
        val N = m.rows
        val lwork = scala.math.max(1, N)
        val work = Array.ofDim[Double](lwork)
        val info = new intW(0)
        lapack.dgetri(
          N,
          m.data,
          scala.math.max(1, N) /* LDA */,
          ipiv,
          work /* workspace */,
          lwork /* workspace size */,
          info
        )
        assert(info.`val` >= 0, "Malformed argument %d (LAPACK)".format(-info.`val`))

        if (info.`val` > 0)
          throw new MatrixSingularException

        m
      }
    }
  }

  implicit def canInvUsingLU_Float[T](implicit
      luImpl: LU.primitive.Impl[T, (DenseMatrix[Float], Array[Int])]
  ): Impl[T, DenseMatrix[Float]] = {
    new Impl[T, DenseMatrix[Float]] {
      def apply(X: T): DenseMatrix[Float] = {
        // Should these type hints be necessary?
        val (m: DenseMatrix[Float], ipiv: Array[Int]) = LU.primitive(X)
        val N = m.rows
        val lwork = scala.math.max(1, N)
        val work = Array.ofDim[Float](lwork)
        val info = new intW(0)
        lapack.sgetri(
          N,
          m.data,
          scala.math.max(1, N) /* LDA */,
          ipiv,
          work /* workspace */,
          lwork /* workspace size */,
          info
        )
        assert(info.`val` >= 0, "Malformed argument %d (LAPACK)".format(-info.`val`))

        if (info.`val` > 0)
          throw new MatrixSingularException

        m
      }
    }
  }

}

/**
 * Computes the Moore-Penrose pseudo inverse of the given real matrix X.
 *
 *     The pseudo inverse is nothing but the least-squares solution to AX=B,
 * hence:
 *        d/dX 1/2 (AX-B)^2 = A^T (AX-B)
 *  Solving A^T (AX-B) = 0 for X yields
 *        A^T AX = A^T B
 *     =>      X = (A^T A)^(-1) A^T B
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

trait pinvLowPrio { this: pinv.type =>

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
  implicit def implFromTransposeAndSolve[T, TransT, MulRes, Result](implicit
      numericT: T => NumericOps[T],
      trans: CanTranspose[T, TransT],
      numericTrans: TransT => NumericOps[TransT],
      mul: OpMulMatrix.Impl2[TransT, T, MulRes],
      numericMulRes: MulRes => NumericOps[MulRes],
      solve: OpSolveMatrixBy.Impl2[MulRes, TransT, Result]
  ): Impl[T, Result] = {
    new Impl[T, Result] {
      def apply(X: T): Result = {
        (X.t * X) \ X.t
      }
    }
  }

}
