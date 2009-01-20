package scalanlp.data;

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


import scalax.io.Implicits._;

import process._;


/**
* Represents a Document of Words. A document consists of one or more ordered 
* fields of text, such as body, and subject.
*
* @author dlwh
*/
class Document[W](val id:String, val fields: Map[String,Seq[W]]) extends Observation[Map[String,Seq[W]]] {
  def features = fields;
   
  def withLabel[L](l:L) = new LabeledDocument[L,W](id,l,fields);
}

class LabeledDocument[L,W](id:String,val label:L, fields: Map[String,Seq[W]]) extends 
  Document[W](id,fields) with Example[L,Map[String,Seq[W]]];

object Document {
  def parseEmail(id:String, lines: Iterator[String]) = {
    var result = Map[String,Seq[String]]();
    var stop = false;
    while(lines.hasNext && !stop) {
      val line = lines.next;
      if(line.trim.length == 0) {
        stop = true;
      } else {
        val colon = line.indexOf(':');
        val category = line.substring(0,colon);
        val tokens = WhitespaceTokenize(line.substring(colon+1)).collect;
        result += (category->tokens);
      }
    }

    if(lines.hasNext) result += ("body"->lines.flatMap(WhitespaceTokenize).collect);

    new Document(id, result);
  }
}
