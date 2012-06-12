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

import generic.Mapper;

import breeze.collection.LazyIterable;

/**
 * Selects the given column as the identifier column for all data rows.
 */
case class IDColumn[ID:Manifest](col : Int) extends Stage[LazyIterable[Item[ID,Seq[String]]],LazyIterable[Item[String,Seq[String]]]] {
  if (col < 1)
    throw new IllegalArgumentException("Column number must be >= 1");
  
  def setID(in : Item[ID,Seq[String]]) : Item[String,Seq[String]] =
    in.copy(id = in.value(col-1));

  override def apply(parcel : Parcel[LazyIterable[Item[ID,Seq[String]]]]) : Parcel[LazyIterable[Item[String,Seq[String]]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.map(setID));

  override def toString =
    "IDColumn("+col+")";
}

/**
 * Selects the given column from a sequence of Strings (e.g. fields in a record).
 * This class assumes the given column index is 1-based.
 * 
 * @author dramage
 */
case class Column[ID:Manifest](col : Int) extends Mapper[ID,Seq[String],String] {
  if (col < 1)
    throw new IllegalArgumentException("Column number must be >= 1");
  
  override def map(in : Seq[String]) =
    in(col-1);

  override def toString =
    "Column("+col+")";
}

/**
 * Selects for only the given column numbers.

 * @author dramage
 */
case class Columns[ID:Manifest](cols : Int*) extends Mapper[ID,Seq[String],Seq[String]] {
  override def map(in : Seq[String]) =
    cols.map(_ - 1).map(in);

  override def toString =
    "Columns("+cols.mkString(",")+")";
}

/**
 * Merges all columns together into a single string using the given
 * glue character.
 *
 * @author dramage
 */
case class Join[ID:Manifest](glue : String) extends Mapper[ID,Seq[String],String] {
  override def map(in : Seq[String]) =
    in.mkString(glue);

  override def toString =
    "Join("+glue+")";
}
