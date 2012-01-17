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
package io;

import java.io.File;

import scalanlp.collection.LazyIterable;
import scalanlp.pipes.Pipes;
import scalanlp.serialization._;

/**
 * A CSVFile acts as a source of Array[String].  Uses scalanlp.serialization's
 * CSVTableSerialization to correctly handle quoting, escaping, etc.
 *
 * @author dramage
 */
class CSVFile(path : String) extends File(path) with LazyIterable[Seq[String]] {

  def write[V:TableWritable](table : V) =
    CSVTableSerialization.write(this, table);

  def write[V:TableWritable](table : V, columns : List[String]) =
    CSVTableSerialization.write(this, table, columns);

  def read[V:TableReadable] : V =
    CSVTableSerialization.read[V](this);

  override def iterator =
    CSVTableSerialization.read[Iterator[List[String]]](this);

  override def toString =
    "CSVFile(" + TextSerialization.toString(path) + ")";
}

/**
 * Companion object for CSVFiles that includes an implicit conversion
 * to a parcel and a format method that correctly escapes and joins
 * a sequence of strings.
 * 
 * @author dramage
 */
object CSVFile {
  /** CSVFile in the current folder. */
  def apply(name : String)(implicit pipes : Pipes = Pipes.global) =
    new CSVFile(pipes.file(name).getPath);

  /** CSVFile that points to the given file. */
  def apply(file : File) =
    new CSVFile(file.getPath);
  
  /** CSVFile that points to a file within the given base folder. */
  def apply(base : File, name : String) =
    new CSVFile(new File(base, name).getPath); 

  /** Calls file.asParcel. */
  implicit def CSVFileAsParcel(file : CSVFile) =
    scalanlp.stage.Parcel(
      history = scalanlp.stage.History.Origin(file.toString),
      meta = scalanlp.collection.immutable.DHMap() + (file : File),
      data = file.zipWithIndex.map(tup => scalanlp.stage.Item(tup._2, tup._1)));
}
