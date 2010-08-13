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

import scalanlp.pipes.Pipes.global._;

import scalanlp.stage.{Parcel,Batch,History};

/**
 * A TXTFile acts as a source of Strings.
 * 
 * @author dramage
 */
case class TXTFile(path : String) extends File(path) { self =>
  def iterator = this.getLines;

  def lines : Iterable[String] = {
    new Iterable[String] {
      override def iterator = self.iterator;
    };
  }

  def asParcel : Parcel[Batch[String]] =
    Parcel(History.Origin(toString), Batch.fromIterator(() => iterator));

  override def toString =
    "TXTFile(\""+path+"\")";
}

/**
 * Companion object for TXTFiles that includes an implicit conversion
 * to a parcel.
 *
 * @author dramage
 */
object TXTFile {
  /** Calls file.asParcel. */
  implicit def TXTFileAsParcel(file : TXTFile) = file.asParcel;
}
