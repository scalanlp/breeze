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
package scalanlp.stage;
package text;

import java.io.File;

import scala.reflect.Manifest;
import scalala.tensor.counters.Counters._;

import scalanlp.collection.immutable.DHMap;
import scalanlp.serialization.FileSerialization;

import scalanlp.stage.{Parcel,Batch,Stage,Mapper,MetaBuilder};
import scalanlp.util.Index;
import scalanlp.util.TopK;

/**
 * Holds basic statistics from a sequence of documents.
 * 
 * @author dramage
 */
class TermCounts(val index : Index[String], val tf : Array[Int], val df : Array[Int]) {
  def reindexed(oindex : Index[String]) : TermCounts = {
    try {
      val otf = Array.tabulate(oindex.size)(i => tf(index(oindex.get(i))));
      val odf = Array.tabulate(oindex.size)(i => df(index(oindex.get(i))));
      new TermCounts(oindex, otf, odf);
    } catch {
      case ex : ArrayIndexOutOfBoundsException =>
        throw new IllegalArgumentException("Cannot reindex TermCounts with new terms", ex);
    }
  }

  def getDF(term : String) = df(index(term));
  def getTF(term : String) = tf(index(term));

  def filterDF(p : Int=>Boolean) =
    reindexed(Index(index.filter(t => p(df(index(t))))));

  def filterTF(p : Int=>Boolean) =
    reindexed(Index(index.filter(t => p(tf(index(t))))));

  def filterIndex(p : String=>Boolean) =
    reindexed(Index(index.filter(p)));
}

object TermCounts {
  def apply(docs : Iterator[Iterable[String]]) = {
    val ctf = IntCounter[String]();
    val cdf = IntCounter[String]();

    val docTermSet = new scala.collection.mutable.HashSet[String]();

    for (terms <- docs) {
      docTermSet.clear();
      for (term <- terms) {
        ctf(term) += 1;
        docTermSet.add(term);
      }
      for (term <- docTermSet) {
        cdf(term) += 1;
      }
    }

    val index = Index(ctf.keysIterator);
    val tf = Array.tabulate(index.size)(i => ctf(index.get(i)));
    val df = Array.tabulate(index.size)(i => cdf(index.get(i)));

    new TermCounts(index, tf, df)
  }
}

/**
 * Computes basic statistics from a sequence of documents.
 *
 * @author dramage
 */
case class TermCounter() extends MetaBuilder[TermCounts,Batch[Iterable[String]]] {
  override def build(data : Batch[Iterable[String]]) =
    TermCounts(data.values.iterator);

  override def toString = "TermCounter()";
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
    val tc = parcel.meta[TermCounts];

    Parcel(parcel.history + this,
      parcel.meta + tc.filterDF(_ >= minDF),
      parcel.data.map((doc : Iterable[String]) => (doc.filter(term => tc.getDF(term) >= minDF))));
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
    val newMeta = {
      if (parcel.meta.contains[TermCounts]) {
        parcel.meta + parcel.meta[TermCounts].filterIndex(term => !stops.contains(term)) + this
      } else {
        parcel.meta + this;
      }
    }

    Parcel(parcel.history + this, newMeta,
      parcel.data.map((doc : Iterable[String]) => (doc.filter(term => !stops.contains(term)))));
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

    val tc = parcel.meta[TermCounts];
    val stops = TopK(numTerms, tc.index, tc.getTF _).toList;

    Parcel(parcel.history + this,
           parcel.meta + tc.filterIndex(term => !stops.contains(term)) + TermStopListFilter(stops),
           parcel.data.map((doc : Iterable[String]) => (doc.filter(term => !stops.contains(term)))));
  }

  override def toString =
    "TermDynamicStopListFilter("+numTerms+")";
}
