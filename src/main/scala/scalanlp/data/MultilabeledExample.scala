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

/**
* Represents a single example from a collection of data. Intentionally
* overly general.
*
* @author dlwh
*/
trait MultilabeledExample[L,+T] extends Example[Set[L],T] with Multilabeled[L] { outer =>
  def id : String;
  override def toString = {
    "Example { ids =" + id + ", labels = " + labels + ", features = " + features + "}"; 
  }

  override def map[U](f: T=>U) = new MultilabeledExample[L,U] {
    def labels = outer.labels;
    def id = outer.id;
    def features = f(outer.features);
  }

  override def flatMap[U](f: T=>U) = map(f);

}
