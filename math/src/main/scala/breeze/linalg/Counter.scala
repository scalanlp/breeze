package breeze.linalg

import breeze.storage.DefaultArrayValue
import breeze.math.Field
import breeze.generic.CanMapValues

/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dramage, dlwh
 */
trait CounterLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +M<:scala.collection.mutable.Map[K,V],
 +This<:Counter[K,V]]
extends TensorLike[K,V,This] with Serializable { self =>
  def data : M
  def default: V

  def repr = this.asInstanceOf[This]

  override def size = data.size
  def activeSize = data.size

  def isEmpty = data.isEmpty

  def contains(k: K) = data.contains(k)

  override def apply(k : K) = {
    data.get(k) getOrElse default
  }

  def update(k : K, v : V) { data(k) = v }

  def get(k: K) = data.get(k)


  override def keysIterator = data.keysIterator

  override def valuesIterator = data.valuesIterator

  override def iterator = data.iterator


  def activeIterator = iterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = keysIterator
}

trait Counter[@specialized(Int, Long) K, @specialized(Int, Float, Double) V] extends Tensor[K,V] with CounterLike[K, V, collection.mutable.Map[K, V], Counter[K,V]] {

}

object Counter {
  class Impl[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
  (override val data : scala.collection.mutable.Map[K,V])
  (implicit defaultArrayValue : DefaultArrayValue[V])
  extends Counter[K,V] {
    def default = defaultArrayValue.value
  }

  /** Returns an empty counter. */
  def apply[K,V:DefaultArrayValue:Field]() : Counter[K,V] =
    new Impl(scala.collection.mutable.HashMap[K,V]())

  /** Returns a counter by summing all the given values. */
  def apply[K,V:DefaultArrayValue:Field](values : (K,V)*) : Counter[K,V] =
    apply(values)

  /** Returns a counter by summing all the given values. */
  def apply[K,V:DefaultArrayValue:Field](values : TraversableOnce[(K,V)]) : Counter[K,V] = {
    val rv = apply[K,V]()
    val field = implicitly[Field[V]]
    values.foreach({ case (k,v) => rv(k) = field.+(v,rv(k)) })
    rv
  }

  /** Counts each of the given items. */
  def count[K](items : TraversableOnce[K]) : Counter[K,Int] = {
    val rv = apply[K,Int]()
    items.foreach(rv(_) += 1)
    rv
  }

  def count[K](items: K*): Counter[K,Int] = count(items)

  implicit def CanMapValuesCounter[K, V, RV:Field:DefaultArrayValue]: CanMapValues[Counter[K, V], V, RV, Counter[K, RV]]
  = new CanMapValues[Counter[K,V],V,RV,Counter[K,RV]] {
    override def map(from : Counter[K,V], fn : (V=>RV)) = {
      val rv = Counter[K,RV]()
      for( (k,v) <- from.iterator) {
        rv(k) = fn(from.data(k))
      }
      rv
    }

    override def mapActive(from : Counter[K,V], fn : (V=>RV)) = {
      val rv = Counter[K,RV]()
      for( (k,v) <- from.activeIterator) {
        rv(k) = fn(from.data(k))
      }
      rv
    }
  }

}

