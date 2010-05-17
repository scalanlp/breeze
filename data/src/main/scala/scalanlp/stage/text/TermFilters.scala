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

import java.io.File;

import scala.reflect.Manifest;
import scalala.tensor.counters.Counters._;

import scalanlp.collection.immutable.DHMap;
import scalanlp.serialization.FileSerialization;

import scalanlp.stage.{Parcel,Batch,Stage,Mapper,MetaBuilder};

/**
 * Holds basic statistics from a sequence of documents.
 * 
 * @author dramage
 */
class TermCounts(docs : Iterator[Iterable[String]]) {
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
case class TermCounter() extends MetaBuilder[TermCounts,Batch[Iterable[String]]] {
  override def build(data : Batch[Iterable[String]]) =
    new TermCounts(data.values.iterator);

  override def toString = "TermCounter()";
}

object TermCounter extends TermCounter {
  /** Runs the TermCounter() stage if TermCounts are not already in the metadata. */
  def runIfMissing(parcel : Parcel[Batch[Iterable[String]]]) : Parcel[Batch[Iterable[String]]] = {
    if (parcel.meta.contains[TermCounts]) {
      parcel;
    } else {
      parcel ~> TermCounter();
    }
  }
}

/**
 * Filters out terms that occur in less than minDF documents.
 * 
 * @author dramage
 */
case class TermMinimumDocumentCountFilter(minDF : Int)
extends Stage[Batch[Iterable[String]],Batch[Iterable[String]]] {
  override def apply(parcel : Parcel[Batch[Iterable[String]]]) : Parcel[Batch[Iterable[String]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");
    val df = parcel.meta[TermCounts].docCounts;
//  override def apply(_parcel : Parcel[Batch[Iterable[String]]]) : Parcel[Batch[Iterable[String]]] = {
//    val parcel = TermCounter.runIfMissing(_parcel);
//    val df = parcel.meta[TermCounts].docCounts;

    Parcel(parcel.history + this, parcel.meta,
           parcel.data.map((doc : Iterable[String]) => (doc.filter(term => df(term) >= minDF))));
  }

  override def toString =
    "TermMinimumDocumentCountFilter("+minDF+")";
}

/**
 * Filters out terms from the given list.
 * 
 * @author dramage
 */
case class TermStopListFilter(stops : List[String])
extends Stage[Batch[Iterable[String]],Batch[Iterable[String]]] {
  override def apply(parcel : Parcel[Batch[Iterable[String]]]) : Parcel[Batch[Iterable[String]]] = {
    Parcel(parcel.history + this, parcel.meta + this,
           parcel.data.map(
             (doc : Iterable[String]) => (doc.filter(term => !stops.contains(term)))
           ));
  }

  override def toString =
    "TermStopListFilter("+stops+")";
}

/**
 * Filters out the top numTerms most frequent terms from the corpus.
 * 
 * @author dramage
 */
case class TermDynamicStopListFilter(numTerms : Int)
extends Stage[Batch[Iterable[String]],Batch[Iterable[String]]] {
  override def apply(parcel : Parcel[Batch[Iterable[String]]]) : Parcel[Batch[Iterable[String]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");
//  override def apply(_parcel : Parcel[Batch[Iterable[String]]]) : Parcel[Batch[Iterable[String]]] = {
//    val parcel = TermCounter.runIfMissing(_parcel);

    val freq = parcel.meta[TermCounts].termCounts;
    val stops = List() ++ freq.maxk(numTerms);

    Parcel(parcel.history + this, parcel.meta + TermStopListFilter(stops),
           parcel.data.map((doc : Iterable[String]) => (doc.filter(term => !stops.contains(term)))));
  }

  override def toString =
    "TermDynamicStopListFilter("+numTerms+")";
}
