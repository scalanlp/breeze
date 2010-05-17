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
 * Dynamic heterogenously ap, a dynamically-backed version of HMap.  Each type has
 * exactly one value of that type, as before, but the type signature no longer
 * holds the map's key set.. Example:
 * <p/>
 * <code>
 *  val map = DHMap() + 3 + "test" + List(1,2) + List("2","3")
 *  assert(map.get[Int] == 3); // ok
 *  map.get[Float] // rutnime error
 *  map.get[List[Int]] // List(1,2)
 *  map.get[List[String]] // List(2,3)
 * </code>
 * 
 * @author dlwh,dramage
 */
class DHMap (private val map: Map[String,Any]) {
  /** Get a value corresponding to the given type from the map.*/
  def get[U](implicit m: Manifest[U]) =
    map.get(m.toString).asInstanceOf[Option[U]].get;

  def contains[U](implicit m : Manifest[U]) : Boolean =
    map.contains(m.toString);

  /** Get a value corresponding to the given type from the map.*/
  def apply[U]()(implicit m: Manifest[U]) = get[U];
  
  /** Add or replace a value with the corresponding *static* type to the map. */
  def +[U](x: U)(implicit m: Manifest[U]) = new DHMap(map + (m.toString->x))
  
  /** 
   * Replace a value with the corresponding *static* type to the map.
   * This is mostly to keep the inferred type as short as possible.
   */
  def -+[U](x:U)(implicit m: Manifest[U]) = new DHMap(map + (m.toString->x));
  
  /**
   * Applys f to this and combines the result with this map.
   */
  def and[U](f: DHMap=>U)(implicit m: Manifest[U]) : DHMap = {
    val newPart = f(this); 
    this.+[U](newPart);
  }
  
  /** Concatenates two maps. values in "other" override values in "this" */
  def ++[U](other: DHMap) : DHMap = {
    new DHMap(this.map ++ other.map);
  }
  
  def keySet = map.keySet;

  def require[U](msg : String)(implicit m : Manifest[U]) {
    if (!map.keySet.contains(m.toString))
      throw new DHMapMissingKeyException(m.toString+" not found: "+msg);
  }

  override def toString = map.mkString("DHMap(",",",")");
}

/**
 * Utilities for constructing DHMaps
 */
object DHMap {
  /**
   * Construct an empty DHMap. Note that this allows you to successfully
   * compile an extraction of "Any" from this map, but it won't succeed 
   * at run time.
   */
  def apply() = new DHMap(Map.empty);
  
  /**
   * Construct an DHMap with one initial value
   */
  def apply[T](x:T)(implicit m: Manifest[T]) = new DHMap(Map.empty + (m.toString->x))
  
  /**
   * Construct an DHMap with two initial values
   */
  def apply[T,T2](x:T, x2:T2)(implicit m: Manifest[T], m2: Manifest[T2]) = {
    new DHMap(Map.empty ++ List( (m.toString->x) , (m2.toString->x2)))
  }
  
  /**
   * Construct an DHMap with three initial values
   */
  def apply[T,T2,T3](x:T, x2:T2, x3: T3)(implicit m: Manifest[T], m2: Manifest[T2], m3: Manifest[T3]) = {
    new DHMap(Map.empty ++ List( (m.toString->x) , (m2.toString->x2), (m3.toString->x3)))
  }
}

class DHMapMissingKeyException(msg : String) extends RuntimeException(msg);
