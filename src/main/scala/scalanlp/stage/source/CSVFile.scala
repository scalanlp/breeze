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

import java.io.File;

import scalanlp.collection.immutable.HTMap;

import scalanlp.stage.{Parcel,Batch,History};

/**
 * Companion object for CSVFiles that includes an implicit conversion
 * to a parcel and a format method that correctly escapes and joins
 * a sequence of strings.
 * 
 * @author dramage
 */
object CSVFile {
  /** Converts a CSV file into a parcel that acts as a source of strings. */
  implicit def iCSVFileToParcel(file : CSVFile) : Parcel[Unit,Batch[Seq[String]]] = {
    Parcel(History.Origin(file.toString), Batch.fromIterable(file.rows));
  }
  
  /** Converts a CSV file into a stage that acts as a net sink. */
  implicit def iCSVFileToStage(file : CSVFile) : Stage[Any,Any,Batch[Seq[String]],Batch[Seq[String]]] = {
    import scalara.ra.RA.global.pipes._;
    Stage {
      (input : Batch[Seq[String]]) => {
        input.values.elements.map(format) | file;
        input;
      }
    }
  }
  
  /** Formats the given sequence of strings as well-formed line of CSV. */
  def format(seq : Seq[String]) : String = {
    ( for (field <- seq) yield {
        if (field.contains('\n') || field.contains('\r') || field.contains('\"') || field.contains(",")) {
          "\"" + field.replaceAll("\"","\"\"") + "\"";
        } else {
          field;
        }
      }
    ).mkString(",");
  }
    
}

/**
 * A CSVFile acts as a source of Array[String].  Uses the scalax CSV parser
 * under the hood, which correctly handles multi-line cells and embedded
 * &quot; markers.
 * 
 * @author dramage
 */
case class CSVFile(path : String) extends File(path) {
  private def reader() = {
    val fis = new java.io.BufferedInputStream(
      new java.io.FileInputStream(this));
    
    val is =
      if (path.toLowerCase.endsWith(".gz")) {
        new java.util.zip.GZIPInputStream(fis)
      } else { fis };
  
    new java.io.BufferedReader(new java.io.InputStreamReader(is));
  }
  
  def rows : Iterable[Array[String]] = {
    new Iterable[Array[String]] {
      override def elements =
        new scalax.io.CsvIterator(reader());
    };
  }
  
  override def toString =
    "CSVFile(\""+path+"\")";
}
