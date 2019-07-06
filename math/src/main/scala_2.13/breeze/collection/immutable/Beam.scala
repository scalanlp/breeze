package breeze.collection
package immutable

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder

/**
 * Represents a beam, which is essentially a priority queue
 * with a maximum size.
 *
 * @author dlwh
 */
class Beam[T](val maxSize: Int, xs: T*)(implicit o: Ordering[T])
  extends Iterable[T]
    with IterableOps[T, Iterable, Beam[T]]
    with StrictOptimizedIterableOps[T, Iterable, Beam[T]] {
  outer =>
  assert(maxSize >= 0)
  val heap = trim(BinomialHeap(xs: _*))

  override def size = heap.size
  def this(maxSize: Int)(implicit o: Ordering[T]) = this(maxSize, Nil: _*)(o)

  private def trim(h2: BinomialHeap[T]) = {
    var h = h2
    while (h.size > maxSize) {
      h = h.delMin
    }
    h
  }

  private def cat(h: BinomialHeap[T], x: T) = {
    if (h.size < maxSize) h + x
    else if (o.compare(h.min, x) < 0) h.delMin + x
    else h
  }

  def +(x: T): Beam[T] = new Beam[T](maxSize) {
    override val heap = cat(outer.heap, x)
  }

  def iterator = heap.iterator
  override def toString() = iterator.mkString("Beam(", ",", ")")

  override def equals(other: Any) = other match {
    case b: Beam[T @unchecked] => (maxSize == b.maxSize) && this.iterator.sameElements(b.iterator)
    case _ => false
  }

  def min = heap.head
  def best = heap.reduceOption(o.max(_, _))

  // TODO: make this a "real" 2.13 collection

  def map[B](f: T => B)(implicit ev: Ordering[B]): Beam[B] =
    strictOptimizedMap(Beam.canBuildFrom[T, B].newBuilder(this), f)

  def flatMap[B](f: T => IterableOnce[B])(implicit ev: Ordering[B]): Beam[B] =
    strictOptimizedFlatMap(Beam.canBuildFrom[T, B].newBuilder(this), f)

  def collect[B](pf: PartialFunction[T, B])(implicit ev: Ordering[B]): Beam[B] =
    strictOptimizedCollect(Beam.canBuildFrom[T, B].newBuilder(this), pf)

  // TODO: can do better than this when it's a beam
  def concat(that: IterableOnce[T]): Beam[T] = that.iterator.foldLeft(this)(_ + _)
  def ++(that: IterableOnce[T]): Beam[T] = concat(that)

  protected def newBuilder: mutable.Builder[T, Beam[T]] = Beam.canBuildFrom[T, T].newBuilder(this)

  protected override def fromSpecific(coll: IterableOnce[T]): Beam[T] = {
    (newBuilder ++= coll).result()
  }

  override def newSpecificBuilder: scala.collection.mutable.Builder[T,breeze.collection.immutable.Beam[T]] = {
    Beam.canBuildFrom[T, T].newBuilder(this)
  }

  override def empty: Beam[T] = Beam(maxSize)()
}

object Beam {
  def apply[T: Ordering](maxSize: Int)(items: T*): Beam[T] = new Beam(maxSize, items: _*)

  implicit def canBuildFrom[T, U: Ordering]: BuildFrom[Beam[T], U, Beam[U]] = new BuildFrom[Beam[T], U, Beam[U]] {
    def fromSpecific(from: Beam[T])(it: IterableOnce[U]): Beam[U] = (newBuilder(from) ++= it).result()
    def newBuilder(from: Beam[T]): mutable.Builder[U, Beam[U]] =
      new mutable.GrowingBuilder(new breeze.collection.mutable.Beam[U](from.maxSize)).mapResult(b => new Beam[U](b.maxSize) ++ b)
  }
}
