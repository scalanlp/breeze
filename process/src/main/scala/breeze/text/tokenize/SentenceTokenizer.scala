package breeze.text.tokenize

import java.text.BreakIterator

/**
 *
 * @author dlwh
 */

class SentenceTokenizer extends Tokenizer {
  def apply(v1: String): Iterable[String] = {
    new Iterable[String] {
      def iterator: Iterator[String] = new Iterator[String] {
        val it = BreakIterator.getSentenceInstance
        it.setText(v1)
        var start = it.first()
        var end = it.first()
        def hasNext = end >= 0 && end > start || {
          start = end
          end = it.next()
          end != BreakIterator.DONE
        }

        def next(): String = {
          val str = v1.substring(start, end)
          start = end
          str
        }
      }
    }

  }
}

