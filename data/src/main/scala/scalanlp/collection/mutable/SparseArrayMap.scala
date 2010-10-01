package scalanlp.collection.mutable

import scala.collection.generic._;
import scala.reflect.ClassManifest;
import scala.collection.mutable._;

class SparseArrayMap[T:ClassManifest]
(default: =>T, initial:Int=3) extends Map[Int,T] {

  private val arr = new SparseArray[T](Int.MaxValue,default,initial);

  override def apply(i: Int) = arr(i);
  override def update(i: Int, t: T) = { arr(i) = t}
  override def get(i: Int) = arr get i;
  override def contains(i: Int) = arr.contains(i);
  override def +=(kv: (Int,T)) = {update(kv._1,kv._2); this}
  override def -=(i: Int) = {arr -= i; this }

  override def iterator = arr.iterator;
}
