package scalanlp.data;

import scala.collection.Map;
import scalax.io.Implicits._;
import java.io.File;
import counters.Counters._;


/**
* Represents a Bag of (Words). Interfaces should prefer to use
* Example.IntValued[L,W] instead of Bag. This is intended as a
* convenience class.
*
* @author dlwh
*/
class Bag[L,W](val id:String, val label:L, words: Map[W,Int]) extends Example[L,Map[W,Int]] {
  def features = words;
}

object Bag {
  /**
  * Reads in a file as a bag of lower case words. Only alphanumeric characters
  * are read, and word breaks are all those characters that are not those
  * characters. Strings are lowercased. The label is set as the
  * name of the directory containing the file.
  *
  * @author dlwh
  */
  def fromFile(file: File) = {
    val words = 
      for( line <- file.lines;
           word <- line.split("[^A-Za-z0-9]")
         if word != "")
         yield word.toLowerCase;
    val cter = words.elements.acquireFor(count(_));
    new Bag[String,String](file.getName,file.getParentFile.getName,cter);
  }

}
