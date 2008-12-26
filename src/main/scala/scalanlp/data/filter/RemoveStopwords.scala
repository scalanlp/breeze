package scalanlp.data.filter;

import scala.io.Source;
import java.util.Locale;

/**
* Filter that removes stop words. Use the two letter code. "en" or "fr"
* Syntax: words.filter(new RemoveStopwords);
*
* @author dlwh
*/
class RemoveStopwords(val language: String) extends (String=>Boolean) {
  /**
  * Defaults to Locale's language.
  */
  def this() = this(Locale.getDefault.getLanguage);

  val words = {
    val strm = try {
      this.getClass.getClassLoader().getResourceAsStream("stopwords/"+language.toLowerCase+".lst");
    } catch {
      case _ => throw new IllegalArgumentException("Unavailable language: " + language);
    }
    val src = Source.fromInputStream(strm);

    val ret = Set() ++ src.getLines.filter(!_.startsWith("#")).map(_.trim);
    strm.close();
    ret
  }

  def apply(s: String) = !words(s.toLowerCase);

}

object RemoveStopwords extends RemoveStopwords;
