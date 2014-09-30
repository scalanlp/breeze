package breeze.collection.mutable

import collection.generic.CanBuildFrom
import collection.mutable.{ArrayBuffer, Builder, MapLike}
import breeze.storage.Zero
import scala.reflect.ClassTag

/**
 * 
 * @author dlwh
 */
class SparseArrayMap[@specialized T:ClassTag:Zero](val length: Int, default: =>T)
  extends scala.collection.mutable.Map[Int,T] with MapLike[Int,T,SparseArrayMap[T]] with Serializable {
  val array = new SparseArray[T](length)

  def activeSize = array.activeSize

  def defaultValue = default

  override def size = this.activeSize

  override def keysIterator = array.index.iterator

  override def update(i: Int, t: T) = { array.update(i, t) }

  override def iterator =  array.iterator

  override def apply(i: Int) = array.getOrElse(i,default)

  def getOrElseUpdate(i: Int): T = getOrElseUpdate(i, default)

  override def getOrElseUpdate(i: Int, default: =>T): T = array.getOrElseUpdate(i,default)

  override def empty = new SparseArrayMap[T](length,default)

  def +=(kv: (Int, T)):this.type = { update(kv._1,kv._2); this }

  def -=(key: Int) = {update(key,default); this }

  def get(key: Int) = array.get(key)

  def indexAt(i: Int) = array.indexAt(i)
  def valueAt(i: Int) = array.valueAt(i)

}

object SparseArrayMap {
  implicit def canMapValues[T,U:ClassTag:Zero]: CanBuildFrom[SparseArrayMap[T], (Int, U), SparseArrayMap[U]] = new CanBuildFrom[SparseArrayMap[T],(Int,U),SparseArrayMap[U]] {
    def apply(): Builder[(Int, U), SparseArrayMap[U]] = new Builder[(Int,U),SparseArrayMap[U]] {
      var bld = new SparseArrayMap[U](Int.MaxValue,implicitly[Zero[U]].zero)
      def result() = bld

      def clear() {bld = new SparseArrayMap[U](Int.MaxValue,implicitly[Zero[U]].zero)}

      def +=(elem: (Int,U)):this.type = {
        bld(elem._1) = elem._2
        this
      }
    }
    def apply(from: SparseArrayMap[T]): Builder[(Int, U), SparseArrayMap[U]] = apply()
  }
}
