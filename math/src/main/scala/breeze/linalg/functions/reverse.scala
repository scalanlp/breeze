package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.storage.Zero
import scala.reflect.ClassTag

/**
 * Returns a reversed copy of the DenseVector.
 * @author ktakagaki
 */
object reverse extends UFunc {
  implicit def dvReverse[T: ClassTag]: Impl[DenseVector[T], DenseVector[T]] =
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(dv: DenseVector[T]): DenseVector[T] = dv((dv.length - 1) to 0 by -1).copy
    }

  implicit def svReverse[T: ClassTag: Zero]: Impl[SparseVector[T], SparseVector[T]] =
    new Impl[SparseVector[T], SparseVector[T]] {
      def apply(sv: SparseVector[T]): SparseVector[T] = {
        val nIndex = new Array[Int](sv.activeSize)
        var i = 0
        while (i < sv.activeSize) {
          nIndex(i) = sv.length - 1 - sv.index(sv.activeSize - 1 - i)
          i += 1
        }
        new SparseVector[T](nIndex, sv.data.take(sv.activeSize).reverse, sv.activeSize, sv.length)
      }
    }

}
