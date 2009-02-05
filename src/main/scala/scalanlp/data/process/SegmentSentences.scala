package scalanlp.data.process;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

/** Simple Regex-based sentence terminator. Very very far from perfect.
 * Usage: SegmentSentences(text).foreach{println}
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
