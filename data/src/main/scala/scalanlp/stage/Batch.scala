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
package scalanlp.stage;

import scala.reflect.Manifest;

import scalanlp.collection.LazyIterable;

package object stage {
  type Batch[X] = Parcel[LazyIterable[Item[X]]];
}

/**
 * An item represents a single item corresponding to the
 * given numbered item from the origin.
 *
 * @param origin The (original) source of this data item.
 * @param number The number of this item in the origin.
 * @param value The value of this item.
 */
case class Item[+V](number : Int, value : V) {
  def map[O](f : V => O) =
    Item[O](number, f(value));
}

/**
 * A mapper is a stage that transforms the data from an Iterable[Option[I]] to an
 * Iterable[Option[O]] but adds no metadata.  See the MapperN variants for mappers
 * that can read metadata during the mapping process.
 * 
 * @author dramage
 */
abstract class Mapper[I,O](implicit mO : Manifest[LazyIterable[Item[O]]])
extends Stage[LazyIterable[Item[I]],LazyIterable[Item[O]]] {
  /** Transforms the input data without using metadata. */
  def map(row : I) : O;

  /** Calls map. */
  override def apply(parcel : Parcel[LazyIterable[Item[I]]]) : Parcel[LazyIterable[Item[O]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.map(_.map(map)));
}

/**
 * Companion object to Mapper with a static constructor for simple function mappers.
 * 
 * @author dramage
 */
object Mapper {
  def apply[I,O](f : I => O)(implicit mO : Manifest[O]) = new Mapper[I,O] {
    override def map(row : I) : O = f(row);
    override def toString = "Mapper("+f.toString+")";
  }
}

/**
 * A filter is a stage that removes elements from the input Iterable.
 * If the input option is None, the output is None.  If the input is Some(x)
 * and filter(x) is false, replaces that element with None.  Otherwise it
 * reutrns Some(x) unchanged.
 */
abstract class Filter[I](implicit mI : Manifest[LazyIterable[Item[I]]])
extends Stage[LazyIterable[Item[I]],LazyIterable[Item[I]]] {
  /**
   * Filters the input data without using metadata.  If the return value is
   * true, keeps the record.  If false, filters it.
   */
  def filter(row : I) : Boolean;
    
  /** Calls filter. */
  override def apply(parcel : Parcel[LazyIterable[Item[I]]])  : Parcel[LazyIterable[Item[I]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.filter(item => filter(item.value)));
}

/**
 * Companion object to Filter with a static constructor for simple function filters.
 * 
 * @author dramage
 */
object Filter {
  def apply[I](f : I => Boolean)(implicit mI : Manifest[I]) = new Filter[I] {
    override def filter(row : I) = f(row);
    override def toString = "Filter("+f.toString+")";
  }
}

/**
 * Takes the first n elements from the batch.
 * 
 * @author dramage
 */
case class Take[I](n : Int)(implicit mI : Manifest[I])
extends Stage[LazyIterable[Item[I]],LazyIterable[Item[I]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[I]]]) : Parcel[LazyIterable[Item[I]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.take(n));
  override def toString = "Take("+n+")";
}

/**
 * Drops the first n elements from the batch.
 * 
 * @author dramage
 */
case class Drop[I](n : Int)(implicit mI : Manifest[I])
extends Stage[LazyIterable[Item[I]],LazyIterable[Item[I]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[I]]]) : Parcel[LazyIterable[Item[I]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.drop(n));
  override def toString = "Drop("+n+")";
}
