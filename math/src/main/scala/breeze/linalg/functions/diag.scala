package breeze.linalg

import breeze.generic.UFunc
import breeze.math.Semiring
import scala.collection.mutable
import scala.reflect.ClassTag
import breeze.storage.Zero

/**
 * returns a vector along the diagonal of v.
 * Requires a square matrix?
 * @param m the matrix
 * @tparam V
 */
object diag extends UFunc with diagLowPrio2 {

  implicit def diagDVDMImpl[V: ClassTag: Zero]: diag.Impl[DenseVector[V], DenseMatrix[V]] =
    new diag.Impl[DenseVector[V], DenseMatrix[V]] {
      def apply(t: DenseVector[V]): DenseMatrix[V] = {
        val r = DenseMatrix.zeros[V](t.length, t.length)
        diag(r) := t
        r
      }
    }

  implicit def diagDMDVImpl[V]: diag.Impl[DenseMatrix[V], DenseVector[V]] =
    new diag.Impl[DenseMatrix[V], DenseVector[V]] {
      def apply(m: DenseMatrix[V]): DenseVector[V] = {
        require(m.rows == m.cols, "m must be square")
        DenseVector.create(m.data, m.offset, m.majorStride + 1, m.rows)
      }
    }

  implicit def diagCSCSVImpl[V: ClassTag: Zero]: diag.Impl[CSCMatrix[V], SparseVector[V]] =
    new diag.Impl[CSCMatrix[V], SparseVector[V]] {
      def apply(cm: CSCMatrix[V]): SparseVector[V] = {
        require(cm.rows == cm.cols, "CSC Matrix must be square")
        var rc = 0
        val sv = SparseVector.zeros[V](cm.rows)
        while (rc < cm.rows) {
          sv(rc) = cm(rc, rc)
          rc += 1
        }
        sv
      }
    }

  implicit def diagSVCSCImpl[V: ClassTag: Semiring: Zero]: diag.Impl[SparseVector[V], CSCMatrix[V]] =
    new diag.Impl[SparseVector[V], CSCMatrix[V]] {
      def apply(t: SparseVector[V]): CSCMatrix[V] = {
        val r = new CSCMatrix.Builder[V](t.length, t.length)
        t.activeIterator.foreach(iv => r.add(iv._1, iv._1, iv._2))
        r.result(true, true)
      }
    }

}

trait diagLowPrio extends UFunc { this: UFunc => }

trait diagLowPrio2 extends UFunc with diagLowPrio { this: UFunc => }
