package breeze.linalg

import breeze.collection.mutable.Beam
import breeze.generic.UFunc
import breeze.macros.expand
import breeze.util.{ArrayUtil, Sorting, quickSelect}

import scala.collection.compat.immutable.ArraySeq

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
        val data = v.toArray
        val index = ArrayUtil.range(0, data.length)
        Sorting.indirectSort(data, index, 0, data.length)
        ArraySeq.unsafeWrapArray(index)
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

  implicit def argtopkDenseVector[T: Ordering]: Impl2[DenseVector[T], Int, IndexedSeq[Int]] = {
    new Impl2[DenseVector[T], Int, IndexedSeq[Int]] {
      override def apply(v: DenseVector[T], k: Int): IndexedSeq[Int] = {
        implicit val orderingInt: Ordering[Int] = Ordering[T].on((x: Int) => v(x)).reverse
        val ints = ArrayUtil.range(0, v.length)
        if (k != 0) {
          quickSelect
            .implFromOrdering[Int, collection.mutable.IndexedSeq[Int]]
            .apply(ints: collection.mutable.IndexedSeq[Int], k - 1)
        }
        ints.take(k)
      }
    }
  }

}

private[linalg] trait LowPriorityArgTopK {
  implicit def argtopkWithQT[Q, I, V](
                                       implicit qt: Q <:< QuasiTensor[I, V],
                                       ord: Ordering[V]): argtopk.Impl2[Q, Int, IndexedSeq[I]] = {
    new argtopk.Impl2[Q, Int, IndexedSeq[I]] {

      def apply(q: Q, k: Int): IndexedSeq[I] = {
        implicit val ordK: Ordering[I] = ord.on(q.apply)
        val queue = new Beam[I](k)
        queue ++= q.keysIterator
        queue.toIndexedSeq.sorted(ordK.reverse)
      }
    }
  }
}

private[linalg] trait LowPriorityArgSort {
  implicit def argsortQuasiTensorWithOrdering[Q, I, V](
                                                        implicit qt: Q <:< QuasiTensor[I, V],
                                                        ord: Ordering[V]): argsort.Impl[Q, IndexedSeq[I]] = {
    new argsort.Impl[Q, IndexedSeq[I]] {
      def apply(q: Q): IndexedSeq[I] = {
        q.keysIterator.toIndexedSeq.sorted(ord.on[I](q(_)))
      }
    }
  }
}