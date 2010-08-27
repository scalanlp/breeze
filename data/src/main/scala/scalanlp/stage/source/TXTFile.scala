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

import scalanlp.pipes.Pipes;
import scalanlp.collection.LazyIterable;
import scalanlp.stage.{Parcel,Item,History};
import scalanlp.serialization._;

/**
 * A TXTFile acts as a source of Strings.
 * 
 * @author dramage
 */
class TXTFile(path : String) extends File(path) with LazyIterable[String] {
  import Pipes.global._;
  
  override def iterator = this.getLines;

  override def toString =
    "TXTFile(\""+path+"\")";
}

trait FileStreams {
  this : File =>

  def reader() = {
    val fis = new java.io.BufferedInputStream(
      new java.io.FileInputStream(this));

    val is =
      if (getPath.toLowerCase.endsWith(".gz")) {
        new java.util.zip.GZIPInputStream(fis)
      } else { fis };

    new java.io.BufferedReader(new java.io.InputStreamReader(is));
  }

  def printer() = {
    val fos = new java.io.BufferedOutputStream(
      new java.io.FileOutputStream(this));

    val os =
      if (getPath.toLowerCase.endsWith(".gz")) {
        new java.util.zip.GZIPOutputStream(fos)
      } else { fos };

    new java.io.PrintStream(os);
  }
}

/**
 * Companion object for TXTFiles that includes an implicit conversion
 * to a parcel.
 *
 * @author dramage
 */
object TXTFile {
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
  implicit def TXTFileAsParcel(file : TXTFile) =
    Parcel(history = History.Origin(file.toString),
           meta = scalanlp.collection.immutable.DHMap() + (file : File),
           data = file.zipWithIndex.map(tup => Item(tup._2, tup._1)));
}
