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
package collection;

import scala.collection.{IterableLike,TraversableView};
import scala.collection.mutable.Builder;
import scala.collection.generic.{GenericCompanion,CanBuildFrom};
import scala.collection.GenTraversableOnce

import scalanlp.serialization.{DataSerialization,FileSerialization};

import TraversableView.NoBuilder;

/**
 * Implementation trait for lazy iterables that chain computation off an
 * initial function that provides only an interator.  See static constructors
 * in object LazyIterable.
 *
 * @author dramage
 */
trait LazyIterableLike[+A,+This<:LazyIterable[A] with LazyIterableLike[A,This]]
extends Iterable[A] with IterableLike[A,This] {
  self =>

  /** Cannot build new LazyIterables. */
  override protected[this] def newBuilder : Builder[A,This] =
    throw new UnsupportedOperationException(this+".newBuilder");

  override def repr : This = self.asInstanceOf[This];

  override def iterator : Iterator[A];

  override def toIterable : Iterable[A] = LazyIterable.toIterable(repr);

  override def foreach[U](f: A => U): Unit =
    iterator.foreach(f);

  lazy val sizeCache = {
    var result = 0
    for (x <- self) result += 1
    result
  }
  override def size = sizeCache;

  def force[B >: A, That](implicit bf: CanBuildFrom[Iterable[A], B, That]) = {
    val b = bf(toIterable);
    b ++= toIterable;
    b.result();
  }

  trait Transformed[+B] extends LazyIterable[B] {
    override def toString = stringPrefix+"(...)";
  }

  trait Forced[B] extends Transformed[B] {
    protected[this] def forced: Seq[B]
    private[this] lazy val forcedCache = forced
    override def foreach[U](f: B => U) = forcedCache.foreach(f)
    override def iterator = forced.iterator
    override def stringPrefix = self.stringPrefix+"C"
    override def size = forcedCache.size;
  }

  trait Sliced extends Transformed[A] {
    protected[this] val from: Int
    protected[this] val until: Int
    override def foreach[U](f: A => U) {
      var index = 0
      for (x <- self) {
        if (from <= index) {
          if (until <= index) return
          f(x)
        }
        index += 1
      }
    }
    override def iterator = self.iterator slice (from, until)
    override def stringPrefix = self.stringPrefix+"S"
    override def slice(from1: Int, until1: Int): This =
      newSliced(from1 max 0, until1 max 0).asInstanceOf[This]
    override def size = math.min(self.size, until) - from;
  }

  trait Mapped[B] extends Transformed[B] {
    protected[this] val mapping: A => B
    override def foreach[U](f: B => U) {
      for (x <- self)
        f(mapping(x))
    }
    override def iterator = self.iterator map mapping
    override def stringPrefix = self.stringPrefix+"M"
    override def size = self.size;
  }

  trait FlatMapped[B] extends Transformed[B] {
    protected[this] val mapping: A => GenTraversableOnce[B]
    override def foreach[U](f: B => U) {
      for (x <- self)
        for (y <- mapping(x))
          f(y)
    }
    override def iterator = self.iterator flatMap (mapping(_).toIterable.iterator)
    override def stringPrefix = self.stringPrefix+"N"
  }

  trait Appended[B >: A] extends Transformed[B] {
    protected[this] val rest: Traversable[B]
    override def foreach[U](f: B => U) {
      for (x <- self) f(x)
      for (x <- rest) f(x)
    }
    override def iterator = self.iterator ++ rest.toIterable.iterator
    override def stringPrefix = self.stringPrefix+"A"
    override def size = self.size + rest.size;
  }

  trait Filtered extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[U](f: A => U) {
      for (x <- self)
        if (pred(x)) f(x)
    }
    override def iterator = self.iterator filter pred
    override def stringPrefix = self.stringPrefix+"F"
  }

  trait TakenWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[U](f: A => U) {
      for (x <- self) {
        if (!pred(x)) return
        f(x)
      }
    }
    override def iterator = self.iterator takeWhile pred
    override def stringPrefix = self.stringPrefix+"T"
  }

  trait DroppedWhile extends Transformed[A] {
    protected[this] val pred: A => Boolean
    override def foreach[U](f: A => U) {
      var go = false
      for (x <- self) {
        if (!go && !pred(x)) go = true
        if (go) f(x)
      }
    }
    override def iterator = self.iterator dropWhile pred
    override def stringPrefix = self.stringPrefix+"D"
  }

  trait Zipped[B] extends Transformed[(A, B)] {
    protected[this] val other: Iterable[B]
    override def iterator: Iterator[(A, B)] = self.iterator zip other.iterator;
    override def stringPrefix = self.stringPrefix+"Z";
    override def size = {
      if (other.hasDefiniteSize)
        math.min(self.size, other.size);
      else
        self.size;
    }
  }

  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] {
    protected[this] val other: Iterable[B]
    protected[this] val thisElem: A1
    protected[this] val thatElem: B
    override def iterator: Iterator[(A1, B)] =
      self.iterator.zipAll(other.iterator, thisElem, thatElem)
    override def size = math.max(self.size, other.size);
  }

  // builder methods

  protected def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected def newFlatMapped[B](f: A => GenTraversableOnce[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  protected def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] { val other = that }
  protected def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new ZippedAll[A1, B] {
    val other: Iterable[B] = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  }

  // implementation methods

  override def ++[B >: A, That](xs: TraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That =
    newAppended(xs.toTraversable).asInstanceOf[That];

  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That =
    newMapped(f).asInstanceOf[That];

  override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[This, B, That]): That =
    filter(pf.isDefinedAt).map(pf)(bf);

  override def flatMap[B, That](f: A =>  GenTraversableOnce[B])(implicit bf: CanBuildFrom[This, B, That]): That =
    newFlatMapped(f).asInstanceOf[That];

  override def filter(p: A => Boolean): This = newFiltered(p).asInstanceOf[This]
  override def withFilter(p: A => Boolean): This = newFiltered(p).asInstanceOf[This]
  override def partition(p: A => Boolean): (This, This) = (filter(p), filter(!p(_)))
  override def init: This = newSliced(0, size - 1).asInstanceOf[This]
  override def drop(n: Int): This = newSliced(n max 0, Int.MaxValue).asInstanceOf[This]
  override def take(n: Int): This = newSliced(0, n).asInstanceOf[This]
  override def slice(from: Int, until: Int): This = newSliced(from max 0, until).asInstanceOf[This]
  override def dropWhile(p: A => Boolean): This = newDroppedWhile(p).asInstanceOf[This]
  override def takeWhile(p: A => Boolean): This = newTakenWhile(p).asInstanceOf[This]
  override def span(p: A => Boolean): (This, This) = (takeWhile(p), dropWhile(p))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n))

  override def grouped(size: Int): Iterator[This] =
    self.iterator.grouped(size).map(xs => newForced(xs).asInstanceOf[This]);

  override def sliding[B >: A](size: Int, step: Int): Iterator[This] =
    self.iterator.sliding(size).map(xs => newForced(xs).asInstanceOf[This]);


  override def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CanBuildFrom[This, (A1, B), That]): That =
    newZipped(that).asInstanceOf[That];

  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[This, (A1, Int), That]): That =
    newZipped(Stream from 0).asInstanceOf[That];

  override def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[This, (A1, B), That]): That =
    newZippedAll(that, thisElem, thatElem).asInstanceOf[That];

  override def stringPrefix = "LazyIterable"

  override def toString = stringPrefix;
}

/**
 * Base trait for lazily evaluated iterables, which defer computation
 * and can be constructed from only a function that generates an iterator.
 *
 * @author dramage
 */
trait LazyIterable[+A] extends LazyIterableLike[A,LazyIterable[A]] {
  override def companion : GenericCompanion[LazyIterable] = LazyIterable;
}

object LazyIterable extends GenericCompanion[LazyIterable] {
  protected val noBuilder = new NoBuilder();

  /** Creates a lazily evaluated iterable from the given iterator-generating function .*/
  def apply[A](mkIterator : =>Iterator[A]) : LazyIterable[A] = {
    new LazyIterable[A] {
      override def iterator = mkIterator;
    }
  }

  /** Creates a lazily evaluated iterable from the given iterator-generating function and the given size hint.*/
  def apply[A](size : Int)(mkIterator : =>Iterator[A]) : LazyIterable[A] = {
    val inSize = size;
    new LazyIterable[A] {
      override def size = inSize;
      override def iterator = mkIterator;
    }
  }

  implicit def toIterable[A](lz : LazyIterable[A]) : Iterable[A] = new Iterable[A] {
    override def iterator = lz.iterator;
    override def size = lz.size;
  }

  implicit def fromSeq[A](seq : Seq[A]) : LazyIterable[A] =
    LazyIterable(seq.length)(seq.iterator);

  //
  // Methods to play nice with scala collections framework
  //

  /** Cannot build new instances of LazyIterable. */
  override def newBuilder[A]: Builder[A, LazyIterable[A]] =
    noBuilder.asInstanceOf[Builder[A,LazyIterable[A]]];

  implicit def canBuildFrom[A]: CanBuildFrom[LazyIterable[_], A, LazyIterable[A]] = {
    new CanBuildFrom[LazyIterable[_], A, LazyIterable[A]] {
      def apply(from: LazyIterable[_]) =
        noBuilder.asInstanceOf[Builder[A,LazyIterable[A]]];
      def apply() =
        noBuilder.asInstanceOf[Builder[A,LazyIterable[A]]];
    }
  }

  /** Reader for cached files on disk to avoid computing the iterable. */
  implicit def fileReadable[A](implicit r : DataSerialization.Readable[A])
  : FileSerialization.Readable[LazyIterable[A]]
  = new FileSerialization.Readable[LazyIterable[A]] {
    override def read(path : java.io.File) = LazyIterable[A] {
      new Iterator[A] {
        val from = new java.io.DataInputStream(io.FileStreams.input(path));
        var remaining = 0;

        override def hasNext = {
          while (remaining == 0) {
            remaining = from.readByte;
            if (remaining < 0) {
              from.close();
            }
          }
          remaining > 0;
        }

        override def next = {
          require(hasNext, "Next called on empty iterator");
          remaining -= 1;
          r.read(from);
        }
      }
    }
  }

  /** Writer for cached files on disk to avoid computing the iterable. */
  implicit def fileWritable[A](implicit w : DataSerialization.Writable[A])
  : FileSerialization.Writable[LazyIterable[A]]
  = new FileSerialization.Writable[LazyIterable[A]] {
    override def write(path : java.io.File, coll : LazyIterable[A]) = {
      val to = new java.io.DataOutputStream(io.FileStreams.output(path));
      val iter = coll.iterator;
      while (iter.hasNext) {
        val values = iter.take(100).toList;
        to.writeShort(values.length.toByte);
        for (value <- values) {
          w.write(to, value);
        }
      }
      to.writeByte(-1);
      to.close();
    }
  }
}
