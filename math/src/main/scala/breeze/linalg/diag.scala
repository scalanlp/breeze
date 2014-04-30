package breeze.linalg

import breeze.generic.UFunc
import scala.reflect.ClassTag
import breeze.storage.DefaultArrayValue

/**
 * returns a vector along the diagonal of v.
 * Requires a square matrix?
 * @param m the matrix
 * @tparam V
 */
object diag extends UFunc with diagLowPrio2 {

  implicit def diagDVDMImpl[V:ClassTag:DefaultArrayValue]:diag.Impl[DenseVector[V], DenseMatrix[V]] = new diag.Impl[DenseVector[V], DenseMatrix[V]] {
    def apply(t: DenseVector[V]): DenseMatrix[V] = {
      val r = DenseMatrix.zeros[V](t.length, t.length)
      diag(r) := t
      r
    }
  }

  implicit def diagDMDVImpl[V]:diag.Impl[DenseMatrix[V], DenseVector[V]] = new diag.Impl[DenseMatrix[V], DenseVector[V]] {
    def apply(m: DenseMatrix[V]): DenseVector[V] = {
      require(m.rows == m.cols, "m must be square")
      new DenseVector(m.data, m.offset, m.majorStride + 1, m.rows)
    }
  }

}

trait diagLowPrio extends UFunc { this: UFunc =>

}


trait diagLowPrio2 extends UFunc with diagLowPrio { this: UFunc =>

}
