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
package scalanlp.stage.text;

import scalanlp.stage.{Batch,Parcel};
import scalanlp.stage.Stage;
import scalanlp.stage.Stage._;

import scalanlp.text.tokenize.Tokenizer;
import scalanlp.serialization.TypedCompanion1;

case class TokenizeWith(tokenizer : Tokenizer)
extends Stage[Batch[String],Batch[Iterable[String]]] {
  override def apply(parcel : Parcel[Batch[String]]) : Parcel[Batch[Iterable[String]]] =
    Parcel(parcel.history + this, parcel.meta + this, parcel.data.map(tokenizer));
}

object TokenizeWith extends TypedCompanion1[Tokenizer,TokenizeWith] {
  prepare();
}

///**
// * A RegexTokenizer tokenizes a string by splitting according
// * to the given regular expression.
// *
// * @author dramage
// */
//@deprecated("Use TokenizeWith(scalanlp.text.tokenizers.RegexTokenizer(pattern))")
//case class RegexTokenizer(pattern : String) extends Mapper[String,Seq[String]] {
//  override def map(doc : String) =
//    doc.split(pattern);
//
//  override def toString =
//    "RegexTokenizer("+pattern+")";
//}
//
///**
// * The SimpleWhitespaceTokenizer tokenizes on one or more
// * space characters.
// *
// * @author dramage
// */
//@deprecated("Use TokenizeWith(scalanlp.text.tokenizers.WhitespaceTokenizer())")
//object WhitespaceTokenizer extends RegexTokenizer("\\s+") {
//  override def toString = "WhitespaceTokenizer";
//}
//
//
///**
// * Simple English document tokenizer pre-processor based on regular
// * expressions from Steven Bethard.
// *
// * @author dramage
// */
//@deprecated("Use TokenizeWith(scalanlp.text.tokenizers.SimpleWhitesapceTokenizer)")
//object SimpleEnglishTokenizer extends Mapper[String,Seq[String]] {
//  override def map(in : String) : Seq[String] = {
//    var string = in;
//    // delete word-final hyphens when followed by newlines
//    string = string.replaceAll("(?<=\\w)-\\s*\n\\s*", "")
//    // add spaces around non-word-internal punctuation
//    string = string.replaceAll("(?<=\\W)(\\p{P})(?! )", "$1 ");
//    string = string.replaceAll("(?! )(\\p{P})(?=\\W)", " $1");
//    return string.split("\\s+");
//  }
//
//  override def toString = "SimpleEnglishTokenizer";
//}
