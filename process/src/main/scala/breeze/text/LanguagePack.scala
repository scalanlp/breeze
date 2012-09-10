package breeze.text

import analyze.{EnglishWordClassGenerator, Analyzer, PorterStemmer, Stemmer}
import segment.{JavaSentenceSegmenter, SentenceSegmenter}
import tokenize.{JavaWordTokenizer, PTBTokenizer, SimpleEnglishTokenizer, Tokenizer}
import transform.{StopWordFilter, Transformer}
import java.util.Locale

/**
 * Provides basic language-specific routines for different languages
 * @author dlwh
 */
trait LanguagePack {
  def stemmer: Option[Stemmer]
  def simpleTokenizer: Tokenizer
  def treebankTokenizer: Option[Tokenizer]
  def sentenceSegmenter: SentenceSegmenter
  def stopwordsFilter: Transformer
  def wordClassGenerator: Option[Analyzer]
}

object LanguagePack {
  object English extends LanguagePack {
    def stemmer: Option[Stemmer] = Some(PorterStemmer)

    def simpleTokenizer: Tokenizer = SimpleEnglishTokenizer()

    def treebankTokenizer: Option[Tokenizer] = Some(PTBTokenizer)

    def sentenceSegmenter: SentenceSegmenter = new JavaSentenceSegmenter(Locale.ENGLISH)

    def stopwordsFilter: Transformer = StopWordFilter(Locale.ENGLISH)

    def wordClassGenerator: Option[Analyzer] = Some(EnglishWordClassGenerator)
  }

  def forLocale(locale: Locale) = {
    if(locale.getLanguage == "en") English
    else new LanguagePack {
      def stemmer: Option[Stemmer] = None

      def simpleTokenizer: Tokenizer = new JavaWordTokenizer(locale)

      def treebankTokenizer: Option[Tokenizer] = None

      def sentenceSegmenter: SentenceSegmenter = new JavaSentenceSegmenter(locale)

      def stopwordsFilter: Transformer = try {
        StopWordFilter(locale)
      } catch {
        case ex: IllegalArgumentException =>
        new Transformer {
          def apply(v1: Iterable[String]): Iterable[String] = v1
        }
      }

      def wordClassGenerator: Option[Analyzer] = None
    }

  }
}
