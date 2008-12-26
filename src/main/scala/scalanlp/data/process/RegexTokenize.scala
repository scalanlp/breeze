package scalanlp.data.process;

import scala.util.matching._;

/**
* Returns all tokens that match the regex as an iterator.
*/
class RegexTokenize(val regex: Regex) extends (String=>Iterator[String]) {
  def this(s:String) = this(s.r);

  def apply(s:String): Iterator[String] = {
    regex.findAllIn(s);
  }
}

object AlphanumericTokenize extends RegexTokenize("[A-Za-z0-9]+");
