package scalanlp.data;

import scalax.io.Implicits._;
import java.io.File;
import counters.Counters._;


/**
* Represents a sequence of text. This is just a string with an id, a nice map method.
*
* @author dlwh
*/
class Text(val id:String, val text: String) extends Observation[String] {
  def features = text;
   
  def withLabel[L](l:L) = new LabeledText[L](id,l,text);
}

class LabeledText[L](id:String,val label:L, fields: String) extends Text(id,fields) with Example[L,String];
