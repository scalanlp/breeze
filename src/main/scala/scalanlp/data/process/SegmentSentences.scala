package scalanlp.data.process;


/** Simple Regex-based sentence terminator. Very vary from perfect. Based on 
* http://en.wikipedia.org/wiki/Sentence_boundary_disambiguation#Strategies
*
* @author dlwh
*/
object SegmentSentences extends (String=>Iterator[String]) {
  private val r = "(.+?)(((?<=[a-z0-9)][.?!])|(?<=[a-z0-9][.?!]\"))(\\s|\r?\n)(?=\"?[A-Z])|$)".r
  def apply(s:String):Iterator[String] = r findAllIn s;
}
