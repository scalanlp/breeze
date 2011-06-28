package scalanlp.collection.mutable

import collection.generic.CanBuildFrom
import collection.mutable.{ArrayBuffer, Builder, MapLike}
import scalala.collection.sparse.{DefaultArrayValue, SparseArray}

/**
 * 
 * @author dlwh
 */
class SparseArrayMap[@specialized T:ClassManifest:DefaultArrayValue](val length: Int, default: =>T)
  extends scala.collection.mutable.Map[Int,T] with IntMap[T] with MapLike[Int,T,SparseArrayMap[T]] with Serializable {
  val array = new SparseArray[T](length);

  def activeSize = array.activeLength;

  def defaultValue = default

  override def size = activeSize;

  override def keysIterator = array.indexArray.iterator;

  override def update(i: Int, t: T) = { array.update(i, t); }

  override def iterator = array.activeIterator;

  override def apply(i: Int) = array.getOrElse(i,default);

  def getOrElseUpdate(i: Int): T = getOrElseUpdate(i, default);

  override def getOrElseUpdate(i: Int, default: =>T): T = array.getOrElseUpdate(i,default);

  override def empty = new SparseArrayMap[T](length,default);

  def +=(kv: (Int, T)):this.type = { update(kv._1,kv._2); this }

  def -=(key: Int) = {update(key,default); this }

  def get(key: Int) = array.get(key);

  def indexAt(i: Int) = array.indexAt(i);
  def valueAt(i: Int) = array.valueAt(i);

}

object SparseArrayMap {
  implicit def canMapValues[T,U:ClassManifest:DefaultArrayValue]: CanBuildFrom[SparseArrayMap[T], (Int, U), SparseArrayMap[U]] = new CanBuildFrom[SparseArrayMap[T],(Int,U),SparseArrayMap[U]] {
    def apply(): Builder[(Int, U), SparseArrayMap[U]] = new Builder[(Int,U),SparseArrayMap[U]] {
      var bld = new SparseArrayMap[U](Int.MaxValue,implicitly[DefaultArrayValue[U]].value);
      def result() = bld

      def clear() {bld = new SparseArrayMap[U](Int.MaxValue,implicitly[DefaultArrayValue[U]].value)}

      def +=(elem: (Int,U)):this.type = {
        bld(elem._1) = elem._2;
        this;
      }
    }
    def apply(from: SparseArrayMap[T]): Builder[(Int, U), SparseArrayMap[U]] = apply()
  }
}