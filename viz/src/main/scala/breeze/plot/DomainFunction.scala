package breeze.plot

import breeze.linalg.QuasiTensor

/**
 * Represents a "function" T that knows its (finite) domain,
 * and can also be applied. Typeclass.
 * @author dlwh
 */
trait DomainFunction[T,K,+V] {
  def domain(t: T):IndexedSeq[K]
  def apply(t: T, k: K):V
}

object DomainFunction {
  implicit def arrIsDomainFunction[V]:DomainFunction[Array[V],Int,V] = {
    new DomainFunction[Array[V], Int, V] {
      def domain(t: Array[V]): IndexedSeq[Int] = 0 until t.length

      def apply(t: Array[V], k: Int): V = t(k)
    }
  }

  implicit def quasitensorIsDomainFunction[T,K,V](implicit ev: T<:<QuasiTensor[K,V]):DomainFunction[T,K,V] = new DomainFunction[T, K, V] {
    def domain(t: T): IndexedSeq[K] = t.keySet.toIndexedSeq

    def apply(t: T, k: K): V = t(k)
  }

  implicit def seqIsDomainFunction[T,V](implicit ev: T<:<collection.Seq[V]):DomainFunction[T,Int,V] = {
    new DomainFunction[T,Int,V] {
      def domain(t: T): IndexedSeq[Int] = (0 until t.length)

      def apply(t: T, k: Int): V = {
        t(k)
      }
    }

  }


  implicit def mapIsDomainFunction[T,K,V](implicit ev: T<:<collection.Map[K, V]):DomainFunction[T,K,V] = {
    new DomainFunction[T,K,V] {
      def domain(t: T): IndexedSeq[K] = t.keySet.toIndexedSeq

      def apply(t: T, k: K): V = {
        t(k)
      }
    }

  }
}
