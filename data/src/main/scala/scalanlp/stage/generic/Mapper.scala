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
package scalanlp;
package stage;
package generic;

import scalanlp.collection.LazyIterable;

/**
 * A mapper is a stage that transforms the data from an Iterable[Option[I]] to an
 * Iterable[Option[O]] but adds no metadata.  See the MapperN variants for mappers
 * that can read metadata during the mapping process.
 *
 * @author dramage
 */
abstract class Mapper[ID,I,O](implicit mO : Manifest[LazyIterable[Item[ID,O]]])
extends Stage[LazyIterable[Item[ID,I]],LazyIterable[Item[ID,O]]] {
  /** Transforms the input data without using metadata. */
  def map(row : I) : O;

  /** Calls map. */
  override def apply(parcel : Parcel[LazyIterable[Item[ID,I]]]) : Parcel[LazyIterable[Item[ID,O]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.map(_.map(map)));
}

/**
 * Companion object to Mapper with a static constructor for simple function mappers.
 *
 * @author dramage
 */
object Mapper {
  def apply[ID:Manifest,I,O:Manifest](f : I => O) = new Mapper[ID,I,O] {
    override def map(row : I) : O = f(row);
    override def toString = "Mapper("+f.toString+")";
  }
}
