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
package io;

import java.io.File;

import breeze.pipes.Pipes;
import breeze.collection.LazyIterable;
import breeze.serialization._;

/**
 * A TSV file acts as a source of Array[String] is a simple tab-delimited
 * file with no escaping.  The incoming strings are simply split on tab and
 * outgoing strings have their tabs replaced by a single space character.
 * 
 * @author dramage
 */
class TSVFile(path : String) extends File(path) with LazyIterable[Seq[String]] {

  def write[V:TableWritable](table : V) =
    TSVTableSerialization.write(this, table);

  def read[V:TableReadable] : V =
    TSVTableSerialization.read[V](this);

  override def iterator =
    TSVTableSerialization.read[Iterator[List[String]]](this);

  override def toString =
    "TSVFile(" + TextSerialization.toString(path) + ")";
}

/**
 * Companion object for TSVFiles that includes an implicit conversion
 * to a parcel and a format method that correctly escapes and joins
 * a sequence of strings.
 *
 * @author dramage
 */
object TSVFile {
  /** Named file in the current folder. */
  def apply(name : String)(implicit pipes : Pipes = Pipes.global) =
    new TSVFile(pipes.file(name).getPath);

  /** From file. */
  def apply(file : File) =
    new TSVFile(file.getPath);

  /** From file in directory. */
  def apply(file : File, name : String) =
    new TSVFile(new File(file, name).getPath);

  /** Calls file.asParcel. */
  implicit def TSVFileAsParcel(file : TSVFile) =
    breeze.stage.Parcel(
      history = breeze.stage.History.Origin(file.toString),
      meta = breeze.collection.immutable.DHMap() + (file : File),
      data = file.zipWithIndex.map(tup => breeze.stage.Item(tup._2, tup._1)));
}
