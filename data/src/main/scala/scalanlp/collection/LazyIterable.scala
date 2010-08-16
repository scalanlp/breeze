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

import scala.collection.{IterableLike,IterableViewLike,IterableView};
import scala.collection.TraversableView;
import scala.collection.generic.CanBuildFrom;
import TraversableView.NoBuilder;

/**
 * Implementation trait for lazy iterables that chain computation off an
 * initial function that provides only an interator.  See static constructors
 * in object LazyIterable.
 *
 * @author dramage
 */
trait LazyIterableLike[+A,+This<:LazyIterable[A] with LazyIterableLike[A,This]] {
  self =>

  def repr : This = self.asInstanceOf[This];

  def iterator : Iterator[A];

  val toIterable : Iterable[A] = LazyIterable.toIterable(repr);

  def foreach[U](f: A => U): Unit =
    iterator.foreach(f);

  lazy val sizeCache = {
    var result = 0
    for (x <- self) result += 1
    result
  }
  def size = sizeCache;

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
    protected[this] val mapping: A => Traversable[B]
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
    override def iterator: Iterator[(A, B)] = self.iterator zip other.iterator
    override def stringPrefix = self.stringPrefix+"Z"
    override def size = math.min(self.size, other.size);
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
  protected def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
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

  def ++[B >: A](xs: TraversableOnce[B]): LazyIterable[B] =
    newAppended(xs.toTraversable);

  def map[B](f: A => B): LazyIterable[B] =
    newMapped(f);

  def collect[B](pf: PartialFunction[A, B]) : LazyIterable[B] =
    filter(pf.isDefinedAt).map(pf);

  def flatMap[B](f: A => Traversable[B]): LazyIterable[B] =
    newFlatMapped(f);

  def filter(p: A => Boolean): This = newFiltered(p).asInstanceOf[This]
  def withFilter(p: A => Boolean): This = newFiltered(p).asInstanceOf[This]
  def partition(p: A => Boolean): (This, This) = (filter(p), filter(!p(_)))
  def init: This = newSliced(0, size - 1).asInstanceOf[This]
  def drop(n: Int): This = newSliced(n max 0, Int.MaxValue).asInstanceOf[This]
  def take(n: Int): This = newSliced(0, n).asInstanceOf[This]
  def slice(from: Int, until: Int): This = newSliced(from max 0, until).asInstanceOf[This]
  def dropWhile(p: A => Boolean): This = newDroppedWhile(p).asInstanceOf[This]
  def takeWhile(p: A => Boolean): This = newTakenWhile(p).asInstanceOf[This]
  def span(p: A => Boolean): (This, This) = (takeWhile(p), dropWhile(p))
  def splitAt(n: Int): (This, This) = (take(n), drop(n))

  def grouped(size: Int): Iterator[This] =
    self.iterator.grouped(size).map(xs => newForced(xs).asInstanceOf[This]);

  def sliding[B >: A](size: Int, step: Int): Iterator[This] =
    self.iterator.sliding(size).map(xs => newForced(xs).asInstanceOf[This]);

  def zip[A1 >: A, B](that: Iterable[B]): LazyIterable[(A1,B)] =
    newZipped(that);

  def zipWithIndex: LazyIterable[(A,Int)] =
    zip[A, Int](Stream from 0);

  def zipAll[B, A1 >: A](that: Iterable[B], thisElem: A1, thatElem: B): LazyIterable[(A1,B)] =
    newZippedAll(that, thisElem, thatElem);

  def stringPrefix = "LazyIterable"
}

/**
 * Base trait for lazily evaluated iterables, which defer computation
 * and can be constructed from only a function that generates an iterator.
 *
 * @author dramage
 */
trait LazyIterable[+A] extends LazyIterableLike[A,LazyIterable[A]];

object LazyIterable {
  /** Creates a lazily evaluated iterable from the given iterator-generating function .*/
  def apply[A](iterator : ()=>Iterator[A]) : LazyIterable[A] = {
    val inIterator : (()=>Iterator[A]) = iterator;
    new LazyIterable[A] {
      override def iterator = inIterator();
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
}
