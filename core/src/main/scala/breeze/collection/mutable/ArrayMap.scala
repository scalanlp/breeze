package breeze.collection.mutable;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.MapLike;
import scala.collection.generic._;

/**
* Wraps an ArrayBuffer with a Map. Note the odd behavior for -=
* 
* The key must be nonnegative
*
* Chances are you want to change defValue, which is used to
* fill in blanks if you don't add things consecutively. Otherwise you
* get an Exception.
*
* @author dlwh
*/
@SerialVersionUID(1)
class ArrayMap[@specialized V](defValue: =>V,  private val arr: ArrayBuffer[V]) extends scala.collection.mutable.Map[Int,V]
    with MapLike[Int,V,ArrayMap[V]] with Serializable {
  def this(defValue: =>V = ArrayMap.error) = this(defValue, new ArrayBuffer[V]())
  override def default(i: Int): V = defValue;
  override def apply(i: Int) = {
    if(i < arr.length) {
     arr(i)
    } else {
      update(i,default(i));
      arr(i);
    }
  }
  override def get(i : Int) = if(i < arr.length) Some(arr(i)) else None;
  override def += (k: (Int,V)): this.type = { update(k._1,k._2); this};
  override def clear = arr.clear();

  override def getOrElseUpdate(i: Int, v: =>V) = {
    if(i >= arr.length) {
      update(i,v);
    }
    arr(i)
  }

  override def update(i : Int, v : V) {
    while(i > arr.length) {
      arr += default(arr.length);
    }

    if(i == arr.length) 
      arr += v;
    else arr(i) = v; 
  }

  override def empty = new ArrayMap(defValue);

  /**
  * Note that removing an element in the array simply replaces it with the default(i)
  */
  def -=(i : Int):this.type = { arr(i) = default(i); this}
  override def size = arr.size;
  def iterator = keysIterator zip valuesIterator
  override def keysIterator = (0 until arr.length).iterator;
  override def keys:Set[Int] = new Set[Int] {
    def iterator = keysIterator;

    def -(elem: Int) = if(!contains(elem)) this else Set.empty ++ keys - elem;

    def +(elem: Int) = if(contains(elem)) this else Set.empty ++ keys + elem;

    def contains(i: Int) = i < arr.length && i > 0;
  }
  override def valuesIterator = arr.iterator;


  /**
   * Returns the array we're holding on to.
   */
  def innerArray(implicit w: ClassManifest[V]):Array[V] =  arr.toArray;
}

object ArrayMap {
  def error = throw new NoSuchElementException("Key not found, and no default value");
}
