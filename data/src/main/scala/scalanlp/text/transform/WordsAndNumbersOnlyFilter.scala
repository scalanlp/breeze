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

import scalanlp.serialization.TypedCompanion0;

/**
 * An enumeration over token types (see inner objects to
 * TokenType companion object) based on regex patterns
 * originally defined by Steven Bethard.
 *
 * @author dramage
 */
sealed abstract case class TokenType();

object TokenType {
  case object Number;
  case object Punctuation;
  case object Word;
  case object Other;

  def apply(token : String) = {
    if (token.matches("^.*\\p{N}.*$")) {
      TokenType.Number;
    } else if (token.matches("^[\\p{P}\\p{S}]+$")) {
      TokenType.Punctuation;
    } else if (token.matches("^.*\\p{L}+.*$")) {
      TokenType.Word;
    } else {
      TokenType.Other;
    }
  }
}

/**
 * A filter that only accepts word and number tokens.
 *
 * @author dramage
 */
case class WordsAndNumbersOnlyFilter() extends Transformer {
  val accept = Set(TokenType.Number, TokenType.Word);
  override def apply(terms : Iterable[String]) =
    terms.filter(term => accept.contains(TokenType(term)));
}

object WordsAndNumbersOnlyFilter extends TypedCompanion0[WordsAndNumbersOnlyFilter] {
  prepare();
}
