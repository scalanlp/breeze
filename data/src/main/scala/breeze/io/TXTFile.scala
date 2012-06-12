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
 * A TXTFile acts as a source of Strings.
 * 
 * @author dramage
 */
class TXTFile(path : String) extends File(path) with LazyIterable[String] {
  import Pipes.global._;
  
  override def iterator = this.getLines;

  override def toString =
    "TXTFile(" + TextSerialization.toString(path) + ")";
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
    new TXTFile(pipes.file(name).getPath);

  /** From file. */
  def apply(file : File) =
    new TXTFile(file.getPath);

  /** From file in directory. */
  def apply(file : File, name : String) =
    new TXTFile(new File(file, name).getPath);

  /** Calls file.asParcel. */
  implicit def TXTFileAsParcel(file : TXTFile) =
    breeze.stage.Parcel(
      history = breeze.stage.History.Origin(file.toString),
      meta = breeze.collection.immutable.DHMap() + (file : File),
      data = file.zipWithIndex.map(tup => breeze.stage.Item(tup._2, tup._1)));
}
