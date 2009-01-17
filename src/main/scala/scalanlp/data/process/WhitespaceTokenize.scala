package scalanlp.data.process;
import scalanlp.util.JavaCollections._;
import scala.collection.mutable._;

import java.io.BufferedInputStream;
import scala.io.Source;


/**
* Breaks a string into whitespace components. Uses StringTokenizer
*
* @author dlwh
*/
object WhitespaceTokenize extends (String=>Iterator[String]) {
  def apply(s:String): Iterator[String] = {
    new java.util.StringTokenizer(s).map(_.asInstanceOf[String]);
  }

  def apply(src: Source): Iterator[String] = new Iterator[String] {
    private var nextString : Option[String] = None;
    def hasNext = {
      src.hasNext && !nextString.orElse(updateString).isEmpty;
    }

    private def updateString = {
      if(!src.hasNext) {
        nextString = None;
        nextString;
      }

      val b = new StringBuilder();

      var stop = false;

      while(src.hasNext && !stop) {
        val ch = src.next;
        if(!ch.isWhitespace) {
          stop = true;
          b += ch;
        }
      }

      stop = false;
      while(src.hasNext && !stop) {
        val ch = src.next;
        if(ch.isWhitespace) {
          stop = true;
        } else { 
          b += ch;
        }
      }
      if(b.isEmpty)
        nextString = None
      else nextString = Some(b.toString);
      nextString
    }

    def next = {
      val ret = nextString.orElse(updateString).get
      nextString = None;
      ret;
    }

    
  }
}


/**
* Breaks a string into alphabetic components. Uses StringTokenizer
*
* @author dlwh
*/
object AlphaTokenize extends (String=>Array[String]) {
  def apply(s:String) = {
    s.split("[^A-Za-z]");
  }
}
