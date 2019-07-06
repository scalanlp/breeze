package breeze.collection.mutable

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

import java.util.Comparator

import breeze.collection.mutable.Beam.{BeamResult, NotAdded}
import breeze.linalg.clip

import scala.jdk.CollectionConverters._
import scala.collection.generic._
import scala.collection.mutable.ArrayBuffer
import scala.collection._


/**
 * Represents a beam, which is essentially a priority queue
 * with a maximum size.
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class Beam[T](val maxSize: Int)(implicit override protected val ordering: Ordering[T])
    extends Iterable[T]
    with IBeam[T]
    with IterableOps[T, Iterable, Beam[T]]
    with Serializable {
  assert(maxSize >= 0)
  protected val queue = new java.util.PriorityQueue[T](clip(maxSize, 1, 16), ordering: Comparator[T])

  override def size = queue.size

  def min = {
    if (queue.isEmpty) {
      throw new NoSuchElementException
    } else {
      queue.peek()
    }
  }

  override def addOne(x: T): this.type = {
    if (queue.size < maxSize) {
      queue.add(x)
    } else if (maxSize > 0 && ordering.compare(min, x) < 0) {
      queue.poll()
      queue.add(x)
    }
    this
  }

  /** O(n) */
  override def subtractOne(elem: T): this.type = {
    queue.remove(elem)
    this
  }

  override def checkedAdd(x: T): BeamResult[T] = {
    if (queue.size < maxSize) {
      queue.add(x)
      Beam.NothingEvicted
    } else if (maxSize > 0 && ordering.compare(min, x) < 0) {
      val r = queue.poll()
      queue.add(x)
      Beam.Added(Iterable(r))
    } else {
      NotAdded
    }
  }

  def iterator: Iterator[T] = queue.iterator.asScala
  override def toString(): String = iterator.mkString("Beam(", ",", ")")
  override def clear(): Unit = queue.clear()

  /**
   * Drains the beam and returns the items in biggest-first order
   *
   * @return
   */
  override def result(): IndexedSeq[T] = {
    val r = new ArrayBuffer[T]()
    while (!queue.isEmpty) {
      r += queue.poll()
    }
    r.reverse
  }

  override def equals(obj: Any): Boolean = obj match {
    case x: Beam[T @unchecked] => maxSize == x.maxSize && iterator.sameElements(x.iterator)
    case _ => false
  }

  override def clone(): Beam[T] = new Beam[T](maxSize) ++= this.iterator

  override def newSpecificBuilder: scala.collection.mutable.Builder[T, Beam[T]] = {
    Beam.canBuildFrom[T, T].newBuilder(this)
  }

  protected override def fromSpecific(coll: IterableOnce[T]): Beam[T] = {
    Beam.canBuildFrom[T, T].fromSpecific(this)(coll)
  }

  override def empty: Beam[T] = Beam(maxSize)()
}

object Beam {

  sealed trait BeamResult[+T] {
    def deleted: Iterable[T]
  }
  case object NotAdded extends BeamResult[Nothing] {
    def deleted = Iterable.empty
  }
  case class Added[+T](deleted: Iterable[T]) extends BeamResult[T]

  val NothingEvicted: BeamResult[Nothing] = Added(Iterable.empty)

  implicit def canBuildFrom[T, U: Ordering]: BuildFrom[Beam[T], U, Beam[U]] = new BuildFrom[Beam[T], U, Beam[U]] {
    def fromSpecific(from: Beam[T])(it: IterableOnce[U]): Beam[U] = (newBuilder(from) ++= it).result()
    def newBuilder(from: Beam[T]): mutable.Builder[U, Beam[U]] = new mutable.GrowableBuilder(new Beam[U](from.maxSize))
  }
  def apply[T: Ordering](maxSize: Int)(xs: T*): Beam[T] = new Beam[T](maxSize) ++= xs
}
