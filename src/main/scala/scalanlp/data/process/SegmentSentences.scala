package scalanlp.data.process;


/** Simple Regex-based sentence terminator. Very very from perfect.
*
* @author dlwh
*/
class SegmentSentences extends (String=>Iterator[String] ) {
 // private val r = "(?s)\\s*((?:[^!?.]|(?:\\.((?=\\s*[0-9a-z]))|\\.(?<=Dr|Mrs|Mr|Ms|Prof))?)+)".r
  private val r = "(?s)\\s*((?:[^!?.])+\\s*[.!?]?)".r 
  def apply(s:String):Iterator[String] = new Iterator[String] {
    val m =  (r findAllIn s);
    var buffer:Option[String] = None;
    def hasNext = m.hasNext;
    
    def next: String = {
      val n = quickNext.replaceAll("^\\s*","");
      if(!m.hasNext) n;
      
      if(n endsWith ".") {
        buffer = Some(m.next);
        if(n.endsWith("Dr.") ||
           n.endsWith("Mr.")  ||
           n.endsWith("Mrs.") ||
           n.endsWith("Ms.") ||
           n.endsWith("Prof.")) {
         n + ' ' + next;       
       } else if(buffer.get.apply(0).isDigit 
          || !buffer.get.apply(0).isWhitespace) {
          n + next; 
        } else {
          n;
        }
      } else {
        n
      }
        
    }
    
    private def quickNext = buffer match {
      case Some(s) => buffer = None; s;
      case None => m.next; m.group(1);
    }
    
  }
}

object SegmentSentences extends SegmentSentences;