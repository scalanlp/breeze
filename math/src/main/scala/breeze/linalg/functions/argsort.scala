package breeze.linalg

import breeze.linalg.support.{LowPriorityArgTopK, LowPriorityArgSort}
import breeze.generic.UFunc
import breeze.macros.expand
import breeze.util.{quickSelect, Sorting}

/**
 * Returns a sequence of keys sorted by value
 *
 * @author dlwh
 **/
object argsort extends UFunc with LowPriorityArgSort {
  @expand
  implicit def argsortDenseVector[@expand.args(Int, Double, Float, Long) T]: Impl[DenseVector[T], IndexedSeq[Int]] = {
    new Impl[DenseVector[T], IndexedSeq[Int]] {
      override def apply(v: DenseVector[T]): IndexedSeq[Int] = {
        val arr = VectorBuilder.range(v.length)
        val data = if (v.noOffsetOrStride) v.data else v.toArray
        Sorting.indexSort(arr, 0, arr.length, data)
        arr
      }
    }
  }
}


/**
 * Returns the top k indices with maximum value
 *
 * @author dlwh
 **/
object argtopk extends UFunc with LowPriorityArgTopK {

  implicit def argtopkDenseVector[T:Ordering]: Impl2[DenseVector[T], Int, IndexedSeq[Int]] = {
    new Impl2[DenseVector[T], Int, IndexedSeq[Int]] {
      override def apply(v: DenseVector[T], k: Int): IndexedSeq[Int] = {
        implicit val orderingInt: Ordering[Int] = Ordering[T].on((x: Int) => v(x)).reverse
        val ints = VectorBuilder.range(v.length)
        if (k != 0) {
          quickSelect.implFromOrdering[Int, collection.mutable.IndexedSeq[Int]].apply(ints:collection.mutable.IndexedSeq[Int], k - 1)
        }
        ints.take(k)
      }
    }
  }

}