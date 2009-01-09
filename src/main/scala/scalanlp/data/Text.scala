package scalanlp.data;

import scalax.io.Implicits._;
import java.io.File;
import counters.Counters._;


/**
* Represents a sequence of text. This is just a string with an id, a nice map method.
*
* @author dlwh
*/
case class Text(val id:String, val contents: String) extends Observation[String] {
  def features = contents;
   
  def withLabel[L](l:L) = new LabeledText[L](id,l,contents);
}

object Text {
 def fromFile(f :File) = {
   new Text(f.getName,f.slurp);
 }
}

class LabeledText[L](id:String,val label:L, contents: String) extends Text(id,contents) with Example[L,String];

object LabeledText {
  def fromFile(f:File) = {
    new Text(f.getName,f.slurp).withLabel(f.getParentFile.getName);
  }
}
