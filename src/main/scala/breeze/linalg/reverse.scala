package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import scala.reflect.ClassTag

/**
 * Returns a reversed copy of the DenseVector.
 * @author ktakagaki
 */
object reverse extends UFunc {
  implicit def dvReverse[T:ClassTag]: Impl[DenseVector[T], DenseVector[T]] =
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(dv: DenseVector[T]): DenseVector[T] = dv( (dv.length - 1) to 0 by -1).copy
    }

}

