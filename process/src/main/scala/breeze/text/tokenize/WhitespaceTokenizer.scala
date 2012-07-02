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
package breeze;
package text;
package tokenize;


/**
 * Tokenizes by splitting on the regular expression \s+.
 *
 * @author dramage
 */
class WhitespaceTokenizer() extends RegexSplitTokenizer("\\s+");

object WhitespaceTokenizer {
  def apply() : WhitespaceTokenizer = new WhitespaceTokenizer;

  private val _instance : WhitespaceTokenizer = apply();
  def apply(in : String) : Iterable[String] = _instance(in);

}

//
// Contents below are orriginally from scalalnp.data.process.WhitespaceTokenize.
//

//
//package breeze.text.tokenize;
//
//import scala.collection.mutable._;
//
//import scala.collection.JavaConversions._;
//import scala.io.Source;
//
//
///**
//* Breaks a string into whitespace components. Uses StringTokenizer
//*
//* @author dlwh
//*/
//object WhitespaceTokenize extends (String=>Iterator[String]) {
//  def apply(s:String): Iterator[String] = {
//    new java.util.StringTokenizer(s).map(_.asInstanceOf[String]);
//  }
//
//  def apply(src: Source): Iterator[String] = new Iterator[String] {
//    private var nextString : Option[String] = None;
//    def hasNext = {
//      src.hasNext && !nextString.orElse(updateString).isEmpty;
//    }
//
//    private def updateString = {
//      if(!src.hasNext) {
//        nextString = None;
//        nextString;
//      }
//
//      val b = new StringBuilder();
//
//      var stop = false;
//
//      while(src.hasNext && !stop) {
//        val ch = src.next;
//        if(!ch.isWhitespace) {
//          stop = true;
//          b += ch;
//        }
//      }
//
//      stop = false;
//      while(src.hasNext && !stop) {
//        val ch = src.next;
//        if(ch.isWhitespace) {
//          stop = true;
//        } else {
//          b += ch;
//        }
//      }
//      if(b.isEmpty)
//        nextString = None
//      else nextString = Some(b.toString);
//      nextString
//    }
//
//    def next = {
//      val ret = nextString.orElse(updateString).get
//      nextString = None;
//      ret;
//    }
//
//
//  }
//}
//
//
///**
//* Breaks a string into alphabetic components.
//*
//* @author dlwh
//*/
//object AlphaTokenize extends (String=>Array[String]) {
//  def apply(s:String) = {
//    s.split("[^A-Za-z]").filter(""!=);
//  }
//}
