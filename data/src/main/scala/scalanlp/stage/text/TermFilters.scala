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
package scalanlp;
package stage;
package text;

import scalala.tensor.mutable.Tensor;

import java.io.File;

import scalanlp.collection.LazyIterable;
import scalanlp.ra.Cell;
import scalanlp.util.{Index,TopK};


/**
 * Basic term count statistics for a collection of documents.
 * 
 * @author dramage
 */
trait TermCounts { self =>
  /** Number of documents in corpus. */
  def numDocs : Int;

  /** Index of terms seen in the corpus. */
  def index : Index[String];

  /** Number of times each term has been seen. */
  def tf : Array[Int];

  /** Number of documents in which each term occurs. */
  def df : Array[Int];

  /** View of this collection using a new (possibly smaller) term index. */
  def reindexed(oindex : Index[String]) : TermCounts = {
    try {
      val otf = Array.tabulate(oindex.size)(i => tf(index(oindex.get(i))));
      val odf = Array.tabulate(oindex.size)(i => df(index(oindex.get(i))));
      new TermCounts {
        def numDocs = self.numDocs;
        def index = oindex;
        def tf = otf;
        def df = odf;
      };
    } catch {
      case ex : ArrayIndexOutOfBoundsException =>
        throw new IllegalArgumentException("Cannot reindex TermCounts with new terms", ex);
    }
  }

  /** Returns the number of documents the given term occurs in. */
  def getDF(term : String) = df(index(term));

  /** Returns the number of times the given term occurs accross all documents. */
  def getTF(term : String) = tf(index(term));

  /** Filters this TermCounts by removing terms whose df matches the predicate. */
  def filterDF(p : Int=>Boolean) =
    reindexed(Index(index.filter(t => p(df(index(t))))));

  /** Filters this TermCounts by removing terms whose tf matches the predicate. */
  def filterTF(p : Int=>Boolean) =
    reindexed(Index(index.filter(t => p(tf(index(t))))));

  /** Filters this TermCounts by removing terms that match the predicate. */
  def filterIndex(p : String=>Boolean) =
    reindexed(Index(index.filter(p)));
}

object TermCounts {
  def apply(docs : Iterator[Iterable[String]], cache : Option[File] = None) : TermCounts = {
    new TermCounts {
      private def compute : (Int,Index[String],Array[Int],Array[Int]) = {
        val index = new scalanlp.util.HashIndex[String]();
        val ctf = Tensor[Int,Int]();
        val cdf = Tensor[Int,Int]();

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

        val tf = Array.tabulate(index.size)(i => ctf(i));
        val df = Array.tabulate(index.size)(i => cdf(i));
        (numDocs, index, tf, df)
      }

      /** Generate the actual term counts. */
      protected lazy val literal : (Int,Index[String],Array[Int],Array[Int]) =
        if (cache.isDefined) Cell.cache(cache.get)(compute) else compute;

      override def numDocs = literal._1;

      override def index = literal._2;

      override def tf = literal._3;

      override def df = literal._4;
    }
  }
}

/**
 * Stop list of terms filtered out from the text.
 *
 * @author dramage
 */
case class TermStopList(stops : List[String]) extends Iterable[String] {
  override def iterator = stops.iterator;
}

/**
 * Computes basic statistics from a sequence of documents.
 *
 * @author dramage
 */
case class TermCounter[ID:Manifest]() extends Stage[LazyIterable[Item[ID,Iterable[String]]],LazyIterable[Item[ID,Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[ID,Iterable[String]]]]) : Parcel[LazyIterable[Item[ID,Iterable[String]]]] = {
    val cache : Option[File] = {
      if (parcel.meta.contains[File]) {
        Some(new File(parcel.meta[File].getPath + ".term-counts.cache." + parcel.history.signature + ".gz"));
      } else {
        None;
      }
    }
    val tc = TermCounts(parcel.data.iterator.map(_.value), cache);

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
case class TermMinimumDocumentCountFilter[ID:Manifest](minDF : Int)
extends Stage[LazyIterable[Item[ID,Iterable[String]]],LazyIterable[Item[ID,Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[ID,Iterable[String]]]]) : Parcel[LazyIterable[Item[ID,Iterable[String]]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");
    val tc = parcel.meta[TermCounts];

    Parcel(parcel.history + this,
      parcel.meta + tc.filterDF(_ >= minDF),
      parcel.data.map((doc : Item[ID,Iterable[String]]) => doc.map(_.filter(term => tc.getDF(term) >= minDF))));
  }

  override def toString =
    "TermMinimumDocumentCountFilter("+minDF+")";
}

/**
 * Filters out terms from the given list.
 * 
 * @author dramage
 */
case class TermStopListFilter[ID:Manifest](stops : List[String])
extends Stage[LazyIterable[Item[ID,Iterable[String]]],LazyIterable[Item[ID,Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[ID,Iterable[String]]]]) : Parcel[LazyIterable[Item[ID,Iterable[String]]]] = {
    val newMeta = {
      if (parcel.meta.contains[TermCounts]) {
        parcel.meta + parcel.meta[TermCounts].filterIndex(term => !stops.contains(term)) + TermStopList(stops)
      } else {
        parcel.meta + this;
      }
    }

    Parcel(parcel.history + this, newMeta,
      parcel.data.map((doc : Item[ID,Iterable[String]]) => (doc.map(_.filter(term => !stops.contains(term))))));
  }

  override def toString =
    "TermStopListFilter("+stops+")";
}

/**
 * Filters out the top numTerms most frequent terms from the corpus.
 * 
 * @author dramage
 */
case class TermDynamicStopListFilter[ID:Manifest](numTerms : Int)
extends Stage[LazyIterable[Item[ID,Iterable[String]]],LazyIterable[Item[ID,Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[ID,Iterable[String]]]]) : Parcel[LazyIterable[Item[ID,Iterable[String]]]] = {
    parcel.meta.require[TermCounts]("TermCounter must be run before TermMinimumDocumentCountFilter");

    val tc = parcel.meta[TermCounts];
    val stops = TopK(numTerms, tc.index, tc.getTF _).toList;

    Parcel(parcel.history + this,
           parcel.meta + tc.filterIndex(term => !stops.contains(term)) + TermStopList(stops),
           parcel.data.map((doc : Item[ID,Iterable[String]]) => (doc.map(_.filter(term => !stops.contains(term))))));
  }

  override def toString =
    "TermDynamicStopListFilter("+numTerms+")";
}
