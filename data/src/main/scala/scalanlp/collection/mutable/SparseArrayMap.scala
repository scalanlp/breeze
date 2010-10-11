package scalanlp.collection.mutable

import scala.collection.generic._;
import scala.reflect.ClassManifest;
import scala.collection.mutable._;

/**
 * SparseArrayMap is a mutable Int=>T Map backed by a SparseArray.
 * Unlike most maps, it automatically adds keys when an apply returns the default value.
 */
class SparseArrayMap[T:ClassManifest](default: =>T, initial:Int=3) extends Map[Int,T] {

  private val arr = new SparseArray[T](Int.MaxValue,default,initial);

  /**
   * If the key is not present, SparseArrayMap automatically adds the the default.
   */
  override def apply(i: Int) = arr.getOrElseUpdate(i,default);
  override def update(i: Int, t: T) = { arr(i) = t}
  override def get(i: Int) = arr get i;
  override def contains(i: Int) = arr.contains(i);
  override def +=(kv: (Int,T)) = {update(kv._1,kv._2); this}
  override def -=(i: Int) = {arr -= i; this }

  override def iterator = arr.iterator;

  def getArray = arr;
}
