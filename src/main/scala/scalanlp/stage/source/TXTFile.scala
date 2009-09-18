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

import scalara.ra.RA;

import scalanlp.collection.immutable.HTMap;

import scalanlp.stage.{Parcel,Batch,History};

/**
 * Companion object for TXTFiles that includes an implicit conversion
 * to a parcel.
 * 
 * @author dramage
 */
object TXTFile {
  implicit def iTXTFileToParcel(file : TXTFile) : Parcel[Unit,Batch[String]] = {
    Parcel(History.Origin(file.toString), Batch.fromIterable(file.lines));
  }
}

/**
 * A TXTFile acts as a source of Strings.
 * 
 * @author dramage
 */
case class TXTFile(path : String) extends File(path) {
  import RA.global.pipes._;
  
  def lines : Iterable[String] = {
    new Iterable[String] {
      override def elements =
        for (line <- TXTFile.this.getLines) yield line;
    };
  }
  
  override def toString =
    "TXTFile(\""+path+"\")";
}
