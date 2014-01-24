package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand

/**
 * Returns a reversed copy of the DenseVector.
 * @author ktakagaki
 */
object reverse extends UFunc {
  @expand
  @expand.valify
  implicit def dvReverse[@expand.args(Int, Long, Float, Double) T]: Impl[DenseVector[T], DenseVector[T]] =
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(dv: DenseVector[T]): DenseVector[T] = DenseVector( dv.toArray.reverse )
    }

}

// ???For implicit DenseVector[T].reverse
//
// class DenseVectorReverse[T](override val data: Array[T],
//                            override val offset: Int,
//                            override val stride: Int,
//                            override val length: Int) extends DenseVector[T](data, offset, stride, length) {
//  def reverse(): DenseVector[T] = breeze.linalg.reverse(this)
//}