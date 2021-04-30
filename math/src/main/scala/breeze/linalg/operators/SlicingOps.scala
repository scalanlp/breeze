package breeze.linalg.operators

import breeze.linalg.DenseVector
import breeze.linalg.support.CanSlice
import scalaxy.debug.require

trait DenseVector_SlicingOps extends TensorLowPrio {

  // slicing
  // specialize to get the good class
  implicit def canSlice_DV_Range_eq_DV[@specialized(Int, Float, Long, Double) V]: CanSlice[DenseVector[V], Range, DenseVector[V]] = {
    new CanSlice[DenseVector[V], Range, DenseVector[V]] {
      def apply(v: DenseVector[V], re: Range): DenseVector[V] = {

        val range: Range = re.getRangeWithoutNegativeIndexes(v.length)

        require(range.isEmpty || range.last < v.length)
        require(range.isEmpty || range.start >= 0)
        DenseVector.create(
          v.data,
          offset = v.offset + v.stride * range.start,
          stride = v.stride * range.step,
          length = range.length)
      }
    }
  }

}
