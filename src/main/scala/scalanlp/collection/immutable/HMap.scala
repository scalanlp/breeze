package scalanlp.collection.immutable

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


import scala.reflect.Manifest;
import scala.collection.immutable._;

/**
 * Heterogenously Typed Map, where each type has exactly one value of that
 * type. Example:
 * <p/>
 * <code>
 *  val map = HTMap() + 3 + "test" + List(1,2) + List("2","3")
 *  assert(map.get[Int] == 3); // ok
 *  map.get[Float] // compile time error
 *  map.get[List[Int]] // List(1,2)
 *  map.get[List[String]] // List(2,3)
 * </code>
 * 
 * @author dlwh
 */
class HMap[+T] (private val map: Map[String,Any]) {
  /** Get a value corresponding to the given type from the map.*/
  def get[U>:T](implicit m: Manifest[U]) = map.get(m.toString).asInstanceOf[Option[U]].get;
  
  /** Get a value corresponding to the given type from the map.*/
  def apply[U>:T]()(implicit m: Manifest[U]) = get[U];
  
  /** Add or replace a value with the corresponding *static* type to the map. */
  def +[U](x: U)(implicit m: Manifest[U]) = new HMap[T with U](map + (m.toString->x))
  
  /** 
   * Replace a value with the corresponding *static* type to the map.
   * This is mostly to keep the inferred type as short as possible.
   */
  def -+[U>:T](x:U)(implicit m: Manifest[U]) = new HMap[T] (map + (m.toString->x));
  
  /**
   * Applys f to this and combines the result with this map.
   */
  def and[U](f: HMap[T]=>U)(implicit m: Manifest[U]): HMap[T with U] = {
    val newPart = f(this); 
    this.+[U](newPart);
  }
  
  /** Concatenates two maps. values in "other" override values in "this" */
  def ++[U](other: HMap[U]): HMap[T with U] = {
    new HMap[T with U](this.map ++ other.map);
  }
  
  override def toString = map.mkString("HTMap(",",",")");
}

/**
 * Utilities for constructing HTMaps
 */
object HMap {
  /**
   * Construct an empty HTMap. Note that this allows you to successfully
   * compile an extraction of "Any" from this map, but it won't succeed 
   * at run time.
   */
  def apply() = new HMap[Any](Map.empty);
  
  /**
   * Construct an HTMap with one initial value
   */
  def apply[T](x:T)(implicit m: Manifest[T]) = new HMap[T](Map.empty + (m.toString->x))
  
  /**
   * Construct an HTMap with two initial values
   */
  def apply[T,T2](x:T, x2:T2)(implicit m: Manifest[T], m2: Manifest[T2]) = {
    new HMap[T with T2](Map.empty ++ List( (m.toString->x) , (m2.toString->x2)))
  }
  
  /**
   * Construct an HTMap with three initial values
   */
  def apply[T,T2,T3](x:T, x2:T2, x3: T3)(implicit m: Manifest[T], m2: Manifest[T2], m3: Manifest[T3]) = {
    new HMap[T with T2 with T3](Map.empty ++ List( (m.toString->x) , (m2.toString->x2), (m3.toString->x3)))
  }
}
