package breeze.linalg.support

import breeze.linalg.{QuasiTensor, argsort}

/**
 * TODO
 *
 * @author dlwh
 **/
private[linalg] trait LowPriorityArgSort {
  implicit def argsortQuasiTensorWithOrdering[Q,I,V](implicit ord: Ordering[V], qt: Q<:<QuasiTensor[I, V]) :argsort.Impl[Q, IndexedSeq[I]] = {
    new argsort.Impl[Q, IndexedSeq[I]] {
      def apply(q: Q): IndexedSeq[I] = {
        q.keysIterator.toIndexedSeq.sorted(ord.on[I](q(_)))
      }
    }
  }
}
