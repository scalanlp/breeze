package breeze.collection.mutable

import scala.collection.mutable._

/**
 * AutoUpdater wraps a Map such that any call to apply updates the map with an instance of the default value
 * @author dlwh
 */
class AutoUpdater[M, K, V](val theMap: M, default: => V)(implicit ev: M <:< Map[K, V]) extends Map[K, V] {
  override def apply(k: K) = theMap.getOrElseUpdate(k, default)
  override def update(k: K, v: V) = theMap.update(k, v)

  def +=(kv: (K, V)): this.type = { theMap += kv; this }

  def get(key: K) = theMap.get(key)

  def iterator = theMap.iterator

  def -=(key: K): this.type = { theMap -= key; this }

  override def size = theMap.size
}

object AutoUpdater {
  def apply[M, K, V](map: M, default: => V)(implicit ev: M <:< Map[K, V]): AutoUpdater[M, K, V] =
    new AutoUpdater[M, K, V](map, default)
  def apply[K, V](default: => V): AutoUpdater[Map[K, V], K, V] = apply(HashMap[K, V](), default)
  def ofKeys[K] =
    new {
      def andValues[V](v: => V) = apply[K, V](v)
    }
}
