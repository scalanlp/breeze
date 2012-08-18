package breeze.text.tokenize

/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import java.text._
import java.util.Locale
import breeze.text.segment.SegmentingIterator


/**
 * A Word Segmenter backed by Java's BreakIterator.
 * Given an input string, it will return an iterator over sentences
 * Doesn't return spaces, does return punctuation.
 *
 * @author dlwh
 */

class JavaWordTokenizer(locale: Locale) extends Tokenizer {
  def this() = this(Locale.getDefault)

  def apply(s: String): Iterable[String] = {
    val breaker = BreakIterator.getWordInstance(locale)
    breaker.setText(s)
    new Iterable[String] {
     def iterator = new SegmentingIterator(breaker, s).map(_.trim).filter(!_.isEmpty)
    }
  }
}

object JavaWordTokenizer extends JavaWordTokenizer


