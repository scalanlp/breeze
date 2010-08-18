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

package scalanlp.text.transform;

import scalanlp.serialization.TypedCompanion1;

import scala.io.Source;
import java.util.Locale;

/**
* Filter that removes stop words. Use the two letter code. "en" or "fr"
* Syntax: words.filter(new RemoveStopwords);
*
* @author dramage
* @author dlwh
*/
case class StopWordFilter(language : String) extends Transformer {
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

    val ret = Set() ++ src.getLines().filter(!_.startsWith("#")).map(_.trim);
    strm.close();
    ret
  }

  override def apply(doc : Iterable[String]) =
    doc.filter(word => !words.contains(word.toLowerCase));
 
  def apply(s: String) = !words(s.toLowerCase);

}

object StopWordFilter extends TypedCompanion1[String,StopWordFilter] {
  prepare();

  def apply() = new StopWordFilter();

  override def unpack(swf : StopWordFilter) = swf.language;
}
