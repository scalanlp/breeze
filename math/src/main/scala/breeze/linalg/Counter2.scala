package breeze.linalg

import breeze.linalg.Counter2.Curried
import breeze.storage.DefaultArrayValue
import collection.mutable.HashMap
import breeze.math.Field

/**
 *
 * @author dlwh
 */
/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dlwh
 */
trait Counter2Like
[K1, K2, V,
+M1[VV] <: Curried[scala.collection.mutable.Map,K1]#Result[VV],
+T <: Counter[K2,V],
+This<:Counter2[K1,K2,V]] extends TensorLike[(K1,K2),V,This] { self =>

  def data : M1[_<:T]

  def default: V

  override def size = {
    var s = 0
    for (m <- data.valuesIterator) {
      s += m.size
    }
    s
  }


  def apply(i: (K1, K2)) = apply(i._1, i._2)

  def apply(k : K1, k2: K2) = data.get(k).map(t => t(k2)) getOrElse default

  def contains(k: K1) = data.contains(k)

  def contains(k1: K1, k2: K2) = data.contains(k1) && data(k1).contains(k2)


  def update(i: (K1, K2), v: V) {update(i._1, i._2, v)}

  def update(k1 : K1, k2: K2, v : V) =
    innerGetOrElseUpdate(k1,data)(k2) = v

  private[linalg] def innerGetOrElseUpdate[M](k:K1, m: scala.collection.mutable.Map[K1,M]): M = {
    m.getOrElseUpdate(k,m.default(k))
  }


  override def keysIterator = for ((k1,m) <- data.iterator; k2 <- m.keysIterator) yield (k1,k2)

  override def valuesIterator = for (m <- data.valuesIterator; v <- m.valuesIterator) yield v

  override def iterator = for ((k1,m) <- data.iterator; (k2,v) <- m.iterator) yield (k1,k2)->v

  def activeSize = size

  def activeIterator = iterator
  def activeKeysIterator = keysIterator
  def activeValuesIterator = valuesIterator

  def repr = this.asInstanceOf[This]
}


trait Counter2
[K1, K2, V]
extends Tensor[(K1,K2),V] with Counter2Like[K1,K2,V,Curried[scala.collection.mutable.Map,K1]#Result,Counter[K2,V],Counter2[K1,K2,V]]

object Counter2 {
  class Impl[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V]
  (override val data : scala.collection.mutable.Map[K1,Counter[K2,V]])
  (implicit scalar : DefaultArrayValue[V])
  extends Counter2[K1,K2,V] with Serializable {
    def default = scalar.value
  }

  /** Returns a new empty counter. */
  def apply[K1,K2,V:DefaultArrayValue:Field]() : Counter2[K1,K2,V] = {
    val map = new HashMap[K1,Counter[K2,V]] {
      override def default(k: K1) = Counter[K2,V]()
    }
    new Impl[K1,K2,V](map)
  }


  /** Aggregates the counts in the given items. */
  def apply[K1,K2,V:Field:DefaultArrayValue](values : (K1,K2,V)*) : Counter2[K1,K2,V] =
    apply(values)

  /** Aggregates the counts in the given items. */
  def apply[K1,K2,V:Field:DefaultArrayValue](values : TraversableOnce[(K1,K2,V)]) : Counter2[K1,K2,V] = {
    val rv = apply[K1,K2,V]()
    values.foreach({ case (k1,k2,v) => rv(k1,k2) = implicitly[Field[V]].+(rv(k1,k2), v) })
    rv
  }

  /** Counts the given elements. */
  def count[K1,K2](values : TraversableOnce[(K1,K2)]) : Counter2[K1,K2,Int] = {
    val rv = apply[K1,K2,Int]()
    values.foreach({ case (k1,k2) => rv(k1,k2) += 1; })
    rv
  }

  /**
   * This is just a curried version of scala.collection.Map.
   * Used to get around Scala's lack of partially applied types.
   *
   * @author dlwh
   */
  trait Curried[M[_,_],K] {
    type Result[V] = M[K,V]
  }
}

