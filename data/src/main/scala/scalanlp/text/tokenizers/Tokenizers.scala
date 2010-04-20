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
package scalanlp.text.tokenizers;

import scalanlp.serialization.{SubtypedCompanion,TypedCompanion0,TypedCaseCompanion1};

/**
 * Abstract trait for tokenizers, which act as functions from a String
 * to an Iterable[String].  See companion object for instructions on
 * registering new subtypes outside of the current package.
 *
 * @author dramage
 */
trait Tokenizer extends (String => Iterable[String]);

/**
 * Companion object for Tokenizer that supports automatic TextSerialization
 * of Tokenizer and its subtypes.  Tokenizers not in scalanlp.text.tokenizers
 * need to call <code>Tokenizer.register[CustomTokenizer]("CustomTokenizer")</code>
 * in order for toString and fromString on Tokenizers to recognize the new type.
 *
 * @author dramage
 */
object Tokenizer extends SubtypedCompanion[Tokenizer] {
  prepare();
  register[RegexSplitTokenizer]("RegexSplitTokenizer");
  register[RegexSearchTokenizer]("RegexSearchTokenizer");
  register[WhitespaceTokenizer]("WhitespaceTokenizer");
}

/**
 * Splits the input document according to the given pattern.  Does not
 * return the splits.
 *
 * @author dramage
 */
case class RegexSplitTokenizer(pattern : String) extends Tokenizer {
  override def apply(doc : String) = doc.split(pattern);
}

object RegexSplitTokenizer extends TypedCaseCompanion1[String,RegexSplitTokenizer] {
  prepare();
}


/**
 * Finds all occurrences of the given pattern in the document.
 *
 * @author dlwh
 * @author dramage
 */
case class RegexSearchTokenizer(pattern : String) extends Tokenizer {
  override def apply(doc : String) = new Iterable[String] {
    override def iterator = (pattern.r.findAllIn(doc));
  }
}

object RegexSearchTokenizer extends TypedCaseCompanion1[String,RegexSearchTokenizer] {
  prepare();
}


/**
 * Tokenizes by splitting on the regular expression \s+.
 *
 * @author dramage
 */
class WhitespaceTokenizer() extends RegexSplitTokenizer("\\s+");

object WhitespaceTokenizer extends TypedCompanion0[WhitespaceTokenizer] {
  def apply() = new WhitespaceTokenizer;

  prepare();
}


///**
// * Simple English document tokenizer pre-processor based on regular
// * expressions from Steven Bethard.
// *
// * @author dramage
// */
//case class SimpleEnglishTokenizer() extends Tokenizer;
//
//object SimpleEnglishTokenizer extends SimpleEnglishTokenizer
//with TypedCompanion0[SimpleEnglishTokenizer] {
//  prepare();
//
//  def apply() = V0();
//
//  override def apply(in : String) = apply()(in);
//  override def name = apply().name;
//  override def toString = apply().toString;
//
//  object V0 extends SimpleEnglishTokenizer with TypedCompanion0[V0] {
//    prepare();
//
//    def apply() = this;
//
//    override def apply(in : String) = {
//      var string = in;
//      // delete word-final hyphens when followed by newlines
//      string = string.replaceAll("(?<=\\w)-\\s*\n\\s*", "")
//      // add spaces around non-word-internal punctuation
//      string = string.replaceAll("(?<=\\W)(\\p{P})(?! )", "$1 ");
//      string = string.replaceAll("(?! )(\\p{P})(?=\\W)", " $1");
//      return string.split("\\s+");
//    }
//
//    override def name = "SimpleEnglishTokenizer.V0"
//    override def toString = name;
//  }
//}
