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


import scalala.tensor.counters.Counters.IntCounter;

import java.io.File;

import scalanlp.collection.LazyIterable;
import scalanlp.ra.Cell;
import scalanlp.stage.{Parcel,Stage,MetaBuilder};
import scalanlp.util.Index;
import scalanlp.util.TopK;


/**
 * Holds basic statistics about a sequence of documents.
 *
 * @author dramage
 */
trait TermCounts {
  def index : Index[String];
  def tf : Array[Int];
  def df : Array[Int];

  def reindexed(oindex : Index[String]) : TermCounts = {
    try {
      val otf = Array.tabulate(oindex.size)(i => tf(index(oindex.get(i))));
      val odf = Array.tabulate(oindex.size)(i => df(index(oindex.get(i))));
      new TermCounts.LiteralTermCounts(oindex, otf, odf);
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
  class LiteralTermCounts(
    override val index : Index[String],
    override val tf : Array[Int],
    override val df : Array[Int])
  extends TermCounts;

  class LazyTermCounts(docs : Iterator[Iterable[String]], cache : Option[File])
  extends TermCounts {
    private def compute : (Int,Index[String],Array[Int],Array[Int]) = {
      val index = new scalanlp.util.HashIndex[String]();
      val ctf = IntCounter[Int]();
      val cdf = IntCounter[Int]();

      val docTermSet = scala.collection.mutable.HashSet[Int]();

      var numDocs = 0;
      for (terms <- docs) {
        docTermSet.clear();
        for (term <- terms) {
          val tI = index.index(term);
          ctf(tI) += 1;
          docTermSet.add(tI);
        }
        for (tI <- docTermSet) {
          cdf(tI) += 1;
        }
        numDocs += 1;
      }

      //val index = Index(ctf.keysIterator);
      val tf = Array.tabulate(index.size)(i => ctf(i));
      val df = Array.tabulate(index.size)(i => cdf(i));
      (numDocs, index, tf, df)
    }

    /** Generate the actual term counts. */
    protected lazy val literal : LiteralTermCounts = {
      val computed =
        if (cache.isDefined) Cell.cache(cache.get)(compute) else compute;
      new LiteralTermCounts(computed._2, computed._3, computed._4);
    }

    override def index = literal.index;

    override def tf = literal.tf;

    override def df = literal.df;
  }
}

/**
 * Computes basic statistics from a sequence of documents.
 *
 * @author dramage
 */
case class TermCounter() extends Stage[LazyIterable[Item[Iterable[String]]],LazyIterable[Item[Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[Iterable[String]]]]) : Parcel[LazyIterable[Item[Iterable[String]]]] = {
    val cache : Option[File] = {
      if (parcel.meta.contains[File]) {
        Some(new File(parcel.meta[File].getPath + ".cache." + parcel.history.signature + ".gz"));
      } else {
        None;
      }
    }
    val tc : TermCounts = new TermCounts.LazyTermCounts(parcel.data.iterator.map(_.value), cache);

    Parcel(parcel.history + this,
           parcel.meta + tc,
           parcel.data);
  }

  override def toString = "TermCounter()";
}

/**
 * Filters out terms that occur in less than minDF documents.
 * 
 * @author dramage
 */
case class TermMinimumDocumentCountFilter(minDF : Int)
extends Stage[LazyIterable[Item[Iterable[String]]],LazyIterable[Item[Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[Iterable[String]]]]) : Parcel[LazyIterable[Item[Iterable[String]]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");
    val tc = parcel.meta[TermCounts];

    Parcel(parcel.history + this,
      parcel.meta + tc.filterDF(_ >= minDF),
      parcel.data.map((doc : Item[Iterable[String]]) => doc.map(_.filter(term => tc.getDF(term) >= minDF))));
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
extends Stage[LazyIterable[Item[Iterable[String]]],LazyIterable[Item[Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[Iterable[String]]]]) : Parcel[LazyIterable[Item[Iterable[String]]]] = {
    val newMeta = {
      if (parcel.meta.contains[TermCounts]) {
        parcel.meta + parcel.meta[TermCounts].filterIndex(term => !stops.contains(term)) + this
      } else {
        parcel.meta + this;
      }
    }

    Parcel(parcel.history + this, newMeta,
      parcel.data.map((doc : Item[Iterable[String]]) => (doc.map(_.filter(term => !stops.contains(term))))));
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
extends Stage[LazyIterable[Item[Iterable[String]]],LazyIterable[Item[Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[Iterable[String]]]]) : Parcel[LazyIterable[Item[Iterable[String]]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");

    val tc = parcel.meta[TermCounts];
    val stops = TopK(numTerms, tc.index, tc.getTF _).toList;

    Parcel(parcel.history + this,
           parcel.meta + tc.filterIndex(term => !stops.contains(term)) + TermStopListFilter(stops),
           parcel.data.map((doc : Item[Iterable[String]]) => (doc.map(_.filter(term => !stops.contains(term))))));
  }

  override def toString =
    "TermDynamicStopListFilter("+numTerms+")";
}
