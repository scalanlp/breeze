package scalanlp.data;

import scalax.io.Implicits._;
import java.io.File;
import counters.Counters._;


/**
* Represents a Document of Words. A document consists of one or more ordered 
* fields of text, such as body, and subject.
*
* @author dlwh
*/
class Document[W](val id:String, val fields: Map[String,Seq[W]]) extends Observation[Map[String,Seq[W]]] {
  def features = fields;
  def title = id;

}

class LabeledDocument[L,W](id:String,val label:L, fields: Map[String,Seq[W]]) extends 
  Document[W](id,fields) with Example[L,Map[String,Seq[W]]];
