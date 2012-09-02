package breeze.data

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


import scala.collection.Map

/**
* Represents a single unlabeled example from a collection of data. Intentionally overly general.
*
* @author dlwh
*/
trait Observation[+T] extends Serializable { outer=>
  def id : String
  def features: T

  /**
  * strict, but cached, transformation of features
  */
  def map[U](f: T=>U):Observation[U] = new Observation[U] {
    def id = outer.id
    val features = f(outer.features)
  }

  /**
  * non-strict, but cached, transformation of features
  */
  def flatMap[U](f: T=>U) = map(f)

  override def toString = {
    "Observation { ids =" + id + ", features = " + features + "}"; 
  }
}

object Observation {
  def apply[T](_id: String, _features: T) = new Observation[T] {
   val id = _id
   val features = _features
  }

  /**
  * Lifts a function to operate over Observations,
  * Rather than the contained object.
  */
  def lift[T,U](f: T=>U) = (o : Observation[T]) => o.map(f)
}
