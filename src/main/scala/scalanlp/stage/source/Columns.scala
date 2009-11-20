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
package scalanlp.stage.source;

import scalanlp.stage.Mapper;

/**
 * Selects the given column from a sequence of Strings (e.g. fields in a record).
 * This class assumes the given column index is 1-based.
 * 
 * @author dramage
 */
case class Column(col : Int) extends Mapper[Seq[String],String] {
  if (col < 1)
    throw new IllegalArgumentException("Column number must be >= 1");
  
  override def map(in : Seq[String]) =
    in(col-1);
}

/**
 * Selects for only the given column numbers.

 * @author dramage
 */
case class Columns(cols : Int*) extends Mapper[Seq[String],Seq[String]] {
  override def map(in : Seq[String]) =
    cols.map(_ - 1).map(in);
}

/**
 * Merges all columns together into a single string using the given
 * glue character.
 *
 * @author dramage
 */
case class Join(glue : String) extends Mapper[Seq[String],String] {
  override def map(in : Seq[String]) =
    in.mkString(glue);
}

/**
 * A rich iterable of sequences of strings that can be written to
 * a CSV or TSV file. See constructor methods in companion object.
 * 
 * @author dramage
 */
trait ColumnDataSource extends Iterable[Seq[String]] {
  import scalara.pipes.Pipes.global._;
  
  def | (file : CSVFile) =
    elements.map(CSVFile.format) | file;
  
  def | (file : TSVFile) =
    elements.map(TSVFile.format) | file;
}

object ColumnDataSource {
  def apply(data : =>Iterator[Seq[String]]) = new ColumnDataSource {
    override def elements = data;
  }
  
  def apply(data : Iterable[Seq[String]]) = new ColumnDataSource {
    override def elements = data.elements;
  }
}
