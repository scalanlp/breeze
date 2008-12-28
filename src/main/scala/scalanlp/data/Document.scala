package scalanlp.data;

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
