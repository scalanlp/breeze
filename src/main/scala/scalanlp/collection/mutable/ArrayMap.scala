package scalanlp.collection.mutable;
import scala.collection.mutable.ArrayBuffer;

/**
* Wraps an ArrayBuffer with a Map. Note the odd behavior for -=
* 
* The key must be positive.
*
* Chances are you want to change defaultValue, which is used to
* fill in blanks if you don't add things consecutively. Otherwise you
* get an Exception.
*
* @author dlwh
*/
class ArrayMap[V](private val arr: ArrayBuffer[V]) extends scala.collection.mutable.Map[Int,V] {
  override def default(i: Int): V = defaultValue;
  def defaultValue:V = throw new NoSuchElementException("");
  def this() = this(new ArrayBuffer[V]())
  override def apply(i: Int) = get(i).getOrElse(default(i));
  def get(i : Int) = if(i < arr.length) Some(arr(i)) else None;
  def update(i : Int, v : V) {
    if(i > arr.length)
      arr ++= (arr.length until i) map (default _)

    if(i == arr.length) 
      arr += v;
    else arr(i) = v; 
  }
  /**
  * Note that removing an element in the array <b>downshifts</b>
  * all elements after it.
  */
  def -=(i : Int) { arr remove i;}
  def size = arr.size;
  def elements = keys zip values
  override def keys = (1 to arr.length).elements;
  override def values = arr.elements;

  def innerArray:Array[V] =  arr.toArray;
}
