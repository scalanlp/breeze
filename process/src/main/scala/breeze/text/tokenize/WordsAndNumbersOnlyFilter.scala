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
 * An enumeration over token types (see inner objects to
 * TokenType companion object) based on regex patterns
 * originally defined by Steven Bethard.
 *
 * @author dramage
 */
sealed trait TokenType;

object TokenType {
  abstract class RegexToken(val pattern : java.util.regex.Pattern) extends TokenType {
    def matches(token : String) =
      pattern.matcher(token).matches;
  }

  case object Number extends RegexToken("^.*\\p{N}.*$".r.pattern);
  case object Punctuation extends RegexToken("^[\\p{P}\\p{S}]+$".r.pattern)
  case object Word extends RegexToken("^.*\\p{L}+.*$".r.pattern);
  case object Other extends TokenType;

  def apply(token : String) : TokenType = {
    if (Word.matches(token)) {
      Word;
    } else if (Number.matches(token)) {
      Number;
    } else if (Punctuation.matches(token)) {
      Punctuation;
    } else {
      Other;
    }
  }
}

/**
 * A filter that only accepts word and number tokens.
 *
 * @author dramage
 */
case class WordsAndNumbersOnlyFilter() extends Transformer {
  override def apply(terms : Iterable[String]) =
    terms.filter(term => TokenType.Word.matches(term) || TokenType.Number.matches(term));
}
