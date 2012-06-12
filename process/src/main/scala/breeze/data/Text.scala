package breeze.data;

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

import scala.collection.mutable.StringBuilder;
import scala.io.Source;

import java.io.File;


/**
* Represents a sequence of text. This is just a string with an id, and a nice map method.
*
* @author dlwh
*/
case class Text(id:String, contents: String) extends Observation[String] {
  def features = contents;
   
  def withLabel[L](l:L) = new LabeledText[L](id,l,contents);
}

object Text {
 def fromFile(f :File) = {
   new Text(f.getName,Source.fromFile(f).getLines().toSeq.foldLeft(new StringBuilder)( _ append _).toString);
 }
}

/**
 * A text with a label.
 * @author dlwh
 */
class LabeledText[L](id:String,val label:L, contents: String) extends Text(id,contents) with Example[L,String];

object LabeledText {
  /**
   * Creates a new text with the file as the id, and the directory name as the label.
   */
  def fromFile(f:File) = {
    new Text(f.getName,Source.fromFile(f).getLines().toSeq.foldLeft(new StringBuffer)(_ append _).toString).withLabel(f.getParentFile.getName);
  }
}
