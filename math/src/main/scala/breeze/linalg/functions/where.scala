package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.support.{CanMapKeyValuePairs, CanTraverseKeyValuePairs}
import breeze.math.Semiring
import scala.collection.mutable.ArrayBuffer
import breeze.linalg.support.CanTraverseKeyValuePairs.KeyValuePairsVisitor

/**
 * `where(a)` returns those indices that are non-zero
 *
 * `where(cond, a, b)` returns the value from a if cond is non-zero, and the value from b otherwise
 *
 * @author dlwh
 **/
object where extends UFunc {

  implicit def whereFromTraverseKeyValuePairs[T, K, V](implicit trav: CanTraverseKeyValuePairs[T, K, V], semi: Semiring[V]):Impl[T, IndexedSeq[K]] = {
    new Impl[T, IndexedSeq[K]] {
      override def apply(v: T): IndexedSeq[K] = {
        val result = new ArrayBuffer[K]()
        trav.traverse(v, new KeyValuePairsVisitor[K, V] {
          override def visit(k: K, a: V): Unit = {
            if(a != semi.zero) result += k
          }

          override def visitZeros(numZero: Int, zeroKeys: Iterator[K], zeroValue: V): Unit = {
            if(zeroValue != semi.zero) result ++= zeroKeys
          }
        })

        result
      }
    }
  }

  implicit def where3ArgFromTraverseKeyValuePairs[T, Q, K, V, V2, U](implicit ev: Q <:< QuasiTensor[K, V2], trav: CanMapKeyValuePairs[T, K, V, V2, U], semi: Semiring[V]):Impl3[T, Q, Q, U] = {
    new Impl3[T, Q, Q, U] {

      override def apply(from: T, v2: Q, v3: Q): U = {
        trav.map(from, {(k,v) => if (v != semi.zero) v2(k) else v3(k)})
      }
    }
  }

}
