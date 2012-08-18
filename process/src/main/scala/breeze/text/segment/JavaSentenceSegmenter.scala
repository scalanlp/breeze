package breeze.text.segment

/*
 Copyright 2009 David Hall, Daniel Ramage

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

import java.text.BreakIterator
import java.util.Locale

/**
 * A Sentence Segmenter backed by Java's BreakIterator.
 * Given an input string, it will return an iterator over sentences
 *
 * @author dlwh
 */
class JavaSentenceSegmenter(locale: Locale = Locale.getDefault) extends SentenceSegmenter {
  def apply(s: String): Iterable[String] = {
    val breaker = BreakIterator.getSentenceInstance(locale)
    breaker.setText(s)
    new Iterable[String] {
      def iterator = new SegmentingIterator(breaker, s)
    }
  }
}

object JavaSentenceSegmenter extends JavaSentenceSegmenter(Locale.getDefault)


/**
 * Given a BreakIterator and a string, iterate over the breaks returned by the breakiterator and index into that string
 * for substrings
 */
private[text] class SegmentingIterator(inner: BreakIterator, str: String) extends Iterator[String] {
  private var start = inner.first
  private var end = inner.next

  def hasNext = (end != BreakIterator.DONE)

  def next = {
    val res = str.substring(start, end)
    start = end
    end = inner.next
    res
  }
}