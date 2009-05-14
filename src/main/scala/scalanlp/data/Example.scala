package scalanlp.data;

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


import scala.collection.Map;
import counters._;

/**
* Represents a single example from a collection of data. Intentionally overly general.
*
* @author dlwh
*/
@serializable
trait Example[+L,+T] extends Observation[T] with Labeled[L] {outer=>
  def id : String;
  def label: L

  /** 
   * Converts the features in this example to a different one while still
   * preserving label and id. 
   */
  override def map[U](f: T=>U) = new Example[L,U] {
    def label = outer.label;
    def id = outer.id;
    val features = f(outer.features);
  }

  /** 
   * Converts the features in this example to a different one while still
   * 
   * preserving label and id. 
   */
  override def flatMap[U](f: T=>U) = map(f);

  override def toString = {
    "Example { ids =" + id + ", label = " + label + ", features = " + features + "}"; 
  }
}

object Example {
  /**
  * Lifts a function to operate over Examples,
  * Rather than the contained object.
  */
  def lift[T,U,L](f: T=>U) = (o : Example[L,T]) => o.map(f);
  def apply[L,T](l: L, data: T): Example[L,T] = new Example[L,T] {
    def id = "";
    def label = l;
    def features = data;
  }
}
