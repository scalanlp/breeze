package scalanlp.collection.mutable;

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
import scala.collection.generic._;

/**
* Wraps an ArrayBuffer with a Map. Note the odd behavior for -=
* 
* The key must be positive.
*
* Chances are you want to change defValue, which is used to
* fill in blanks if you don't add things consecutively. Otherwise you
* get an Exception.
*
* @author dlwh
*/
class ArrayMap[V:ClassManifest](private val arr: ArrayBuffer[V]) extends scala.collection.mutable.Map[Int,V]
    with MutableMapTemplate[Int,V,ArrayMap[V]] {
  def this() = this(new ArrayBuffer[V]())
  override def default(i: Int): V = defValue;
  def defValue:V = throw new NoSuchElementException("");
  override def apply(i: Int) = get(i).getOrElse(default(i));
  override def get(i : Int) = if(i < arr.length) Some(arr(i)) else None;
  override def += (k: (Int,V)): this.type = { update(k._1,k._2); this};
  override def update(i : Int, v : V) {
    if(i > arr.length)
      arr ++= (arr.length until i) map (default _)

    if(i == arr.length) 
      arr += v;
    else arr(i) = v; 
  }

  override def empty = new ArrayMap();

  /**
  * Note that removing an element in the array <b>downshifts</b>
  * all elements after it.
  */
  def -=(i : Int):this.type = { arr remove i; this}
  override val size = arr.size;
  def iterator = keys zip values
  override def keys = (0 until arr.length).iterator;
  override def values = arr.iterator;


  def innerArray:Array[V] =  arr.toArray;
}
