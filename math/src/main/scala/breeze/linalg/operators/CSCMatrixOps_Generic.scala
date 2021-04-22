package breeze.linalg.operators

import breeze.linalg.CSCMatrix
import breeze.linalg.support.CanTranspose
import breeze.math.{Complex, Semiring}
import breeze.storage.Zero

import scala.reflect.ClassTag

trait CSCMatrixOps_Generic extends GenericOps with TensorLowPrio {
  implicit def CSC_canTranspose[V: ClassTag: Zero: Semiring]: CanTranspose[CSCMatrix[V], CSCMatrix[V]] = {
    new CanTranspose[CSCMatrix[V], CSCMatrix[V]] {
      def apply(from: CSCMatrix[V]) = {
        val transposedMtx = new CSCMatrix.Builder[V](from.cols, from.rows, from.activeSize)

        var j = 0
        while (j < from.cols) {
          var ip = from.colPtrs(j)
          while (ip < from.colPtrs(j + 1)) {
            val i = from.rowIndices(ip)
            transposedMtx.add(j, i, from.data(ip))
            ip += 1
          }
          j += 1
        }
        // this doesn't hold if there are zeros in the matrix
        //        assert(transposedMtx.activeSize == from.activeSize,
        //          s"We seem to have lost some elements?!?! ${transposedMtx.activeSize} ${from.activeSize}")
        transposedMtx.result(false, false)
      }
    }
  }

  implicit def CSC_canTransposeComplex: CanTranspose[CSCMatrix[Complex], CSCMatrix[Complex]] = {
    new CanTranspose[CSCMatrix[Complex], CSCMatrix[Complex]] {
      def apply(from: CSCMatrix[Complex]) = {
        val transposedMtx = CSCMatrix.zeros[Complex](from.cols, from.rows)

        var j = 0
        while (j < from.cols) {
          var ip = from.colPtrs(j)
          while (ip < from.colPtrs(j + 1)) {
            val i = from.rowIndices(ip)
            transposedMtx(j, i) = from.data(ip).conjugate
            ip += 1
          }
          j += 1
        }
        transposedMtx
      }
    }
  }
}
