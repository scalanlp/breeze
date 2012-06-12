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
package breeze;
package stage;
package generic;

import breeze.collection.LazyIterable;

/**
 * A filter is a stage that removes elements from the input Iterable.
 * If the input option is None, the output is None.  If the input is Some(x)
 * and filter(x) is false, replaces that element with None.  Otherwise it
 * reutrns Some(x) unchanged.
 */
abstract class Filter[ID,I](implicit mI : Manifest[LazyIterable[Item[ID,I]]])
extends Stage[LazyIterable[Item[ID,I]],LazyIterable[Item[ID,I]]] {
  /**
   * Filters the input data without using metadata.  If the return value is
   * true, keeps the record.  If false, filters it.
   */
  def filter(row : I) : Boolean;

  /** Calls filter. */
  override def apply(parcel : Parcel[LazyIterable[Item[ID,I]]])  : Parcel[LazyIterable[Item[ID,I]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.filter(item => filter(item.value)));
}

/**
 * Companion object to Filter with a static constructor for simple function filters.
 *
 * @author dramage
 */
object Filter {
  def apply[ID:Manifest,I:Manifest](name : String, f : I => Boolean) = new Filter[ID,I] {
    override def filter(row : I) = f(row);
    override def toString = "Filter("+serialization.TextSerialization.toString(name)+")";
  }
}
