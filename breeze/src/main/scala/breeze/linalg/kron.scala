package breeze.linalg

import breeze.linalg.operators.OpMulScalar
import scala.reflect.ClassTag
import breeze.storage.DefaultArrayValue
import breeze.generic.UFunc

/**
 * Returns the Kronecker product of two matrices a and b,
 * usually denoted a ⊗ b.
 */
object kron extends UFunc {

  implicit def kronDM_M[V1,V2, M,RV](implicit mul : OpMulScalar.Impl2[V1, M, DenseMatrix[RV]],
                                                    asMat: M<:<Matrix[V2],
                                                    man: ClassTag[RV],
                                                    dfv: DefaultArrayValue[RV]):Impl2[DenseMatrix[V1], M, DenseMatrix[RV]] = {
    new Impl2[DenseMatrix[V1], M, DenseMatrix[RV]] {
      def apply(a: DenseMatrix[V1], b: M): DenseMatrix[RV] = {

        val result: DenseMatrix[RV] = DenseMatrix.zeros[RV](a.rows * b.rows, a.cols * b.cols)

        for( ((r,c),av) <- a.activeIterator) {
          result((r*b.rows) until ((r+1)*b.rows), (c * b.cols) until ((c+1) * b.cols)) := mul(av, b)
        }
        result
      }
    }
  }

}
