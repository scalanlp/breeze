package scalanlp.collection.mutable;
import scala.collection.mutable.ArrayBuffer;

/**
* Wraps an ArrayBuffer with a Map. Note the odd behavior for -=
*
* @author dlwh
*/
class ArrayMap[V](arr: ArrayBuffer[V]) extends scala.collection.mutable.Map[Int,V] {
  def this() = this(new ArrayBuffer[V]())
  override def apply(i: Int) = arr(i);
  def get(i : Int) = if(i < arr.length) Some(arr(i)) else None;
  def update(i : Int, v : V) { arr(i) = v; }
  /**
  * Note that removing an element in the array <b>downshifts</b>
  * all elements after it.
  */
  def -=(i : Int) { arr remove i;}
  def size = arr.size;
  def elements = keys zip values
  override def keys = (1 to arr.length).elements;
  override def values = arr.elements;
}
