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

import scala.reflect.Manifest;
import scalanlp.counters.Counters._;

import scalanlp.stage.{Parcel,Batch,Stage,Mapper,MetaBuilder};

/**
 * Holds basic statistics from a sequence of documents.
 * 
 * @author dramage
 */
class TermCounts(docs : Iterator[Seq[String]]) {
  /** Number of documents each term occurs in. */
  val docCounts  = IntCounter[String]();
    
  /** Number of times each term occurs (across documents). */
  val termCounts = IntCounter[String]();
    
  // initialize counts
  for (terms <- docs) {
    for (term <- terms) {
      termCounts(term) += 1;
    }
    for (term <- Set() ++ terms) {
      docCounts(term) += 1;
    }
  }
}

/**
 * Computes basic statistics from a sequence of documents.
 * 
 * @author dramage
 */
case object TermCounter extends MetaBuilder[TermCounts,Batch[Seq[String]]] {
  override def build(data : Batch[Seq[String]]) =
    new TermCounts(data.values.iterator);
}

/**
 * Filters out terms that occur in less than minDF documents.
 * 
 * @author dramage
 */
case class TermMinimumDocumentCountFilter(minDF : Int)
extends Stage[Batch[Seq[String]],Batch[Seq[String]]] {
  override def apply(parcel : Parcel[Batch[Seq[String]]]) : Parcel[Batch[Seq[String]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");
    val df = parcel.meta[TermCounts].docCounts;

    Parcel(parcel.history + this, parcel.meta,
           parcel.data.map(
             (doc : Seq[String]) => (doc.filter(term => df(term) >= minDF))
          ));
  }
}

/**
 * Filters out terms composed of fewer than minLength characters.
 * 
 * @author dramage
 */
case class TermMinimumLengthFilter(minLength : Int)
extends Mapper[Seq[String],Seq[String]] {
  override def map(doc : Seq[String]) =
    doc.filter(_.length >= minLength);
}

/**
 * Filters out terms from the given list.
 * 
 * @author dramage
 */
case class TermStopListFilter(stops : Set[String])
extends Stage[Batch[Seq[String]],Batch[Seq[String]]] {
  override def apply(parcel : Parcel[Batch[Seq[String]]]) : Parcel[Batch[Seq[String]]] = {
    Parcel(parcel.history + this, parcel.meta + this,
           parcel.data.map(
             (doc : Seq[String]) => (doc.filter(term => !stops.contains(term)))
           ));
  }
}
  
/**
 * Filters out the top numTerms most frequent terms from the corpus.
 * 
 * @author dramage
 */
case class TermDynamicStopListFilter(numTerms : Int)
extends Stage[Batch[Seq[String]],Batch[Seq[String]]] {
  override def apply(parcel : Parcel[Batch[Seq[String]]]) : Parcel[Batch[Seq[String]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");
    val freq = parcel.meta[TermCounts].termCounts;
    val stops = Set() ++ freq.topK(numTerms).map(_._1);

    Parcel(parcel.history + this, parcel.meta + TermStopListFilter(stops),
           parcel.data.map(
            (doc : Seq[String]) => (doc.filter(term => !stops.contains(term)))
          ));
  }
}




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
case object WordsAndNumbersOnlyFilter extends Mapper[Seq[String],Seq[String]] {
  val ok = List(TokenType.Number, TokenType.Word);
  override def map(doc : Seq[String]) : Seq[String] = {
    for (term <- doc; if ok.contains(TokenType(term))) yield term;
  }
}
