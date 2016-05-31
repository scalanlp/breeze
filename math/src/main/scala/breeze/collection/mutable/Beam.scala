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

import breeze.collection.mutable.Beam.{ BeamResult, NotAdded }

import scala.collection.{ mutable, _ }
import scala.collection.generic._
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

trait IBeam[T] extends Iterable[T] with IterableLike[T, IBeam[T]] with mutable.Builder[T, IndexedSeq[T]] with Shrinkable[T] with mutable.Cloneable[IBeam[T]] {
  override protected[this] def newBuilder: mutable.Builder[T, IBeam[T]] = throw new NotImplementedError("This should have been overridden")
  def freshEmpty: IBeam[T] = newBuilder.result()

  /**
    * Returns information on whether or not it made it onto the beam, and also what got
    * evicted
    * @param x
    * @return
    */
  def checkedAdd(x: T): Beam.BeamResult[T]

  def +=(x:T): this.type = {
    checkedAdd(x)
    this
  }
}

/**
  * Represents a beam, which is essentially a priority queue
  * with a maximum size.
  *
  * @author dlwh
  */
@SerialVersionUID(1L)
class Beam[T](val maxSize:Int)(implicit ord: Ordering[T]) extends AbstractIterable[T]
  with IBeam[T]
  with IterableLike[T, Beam[T]]
  with Serializable {
  assert(maxSize >= 0)
  protected val queue = new java.util.PriorityQueue[T](ord)

  override def size = queue.size

  def min = {
    if (queue.isEmpty) {
      throw new NoSuchElementException
    } else {
      queue.peek()
    }
  }

  override def +=(x:T): this.type = {
    if(queue.size < maxSize) {
      queue.add(x)
    } else if (maxSize > 0 && ord.compare(min,x) < 0) {
      queue.poll()
      queue.add(x)
    }
    this
  }

  /** O(n) */
  override def -=(elem: T): this.type = {
    queue.remove(elem)
    this
  }

  override def checkedAdd(x: T): BeamResult[T] = {
    if(queue.size < maxSize) {
      queue.add(x)
      Beam.NothingEvicted
    } else if (maxSize > 0 && ord.compare(min,x) < 0) {
      val r = queue.poll()
      queue.add(x)
      Beam.Added(Iterable(r))
    } else {
      NotAdded
    }
  }

  def iterator: Iterator[T] = queue.iterator.asScala
  override def toString(): String = iterator.mkString("Beam(",",",")")
  override def clear(): Unit = queue.clear()

  override protected def newBuilder: mutable.Builder[T, Beam[T]] = new mutable.GrowingBuilder(new Beam[T](maxSize))

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

  implicit def canBuildFrom[T: Ordering]:CanBuildFrom[Beam[T], T, Beam[T]] = new CanBuildFrom[Beam[T],T,Beam[T]] {
    def apply() = sys.error("Sorry, need a max size")

    def apply(from: Beam[T]) = from.newBuilder
  }
  def apply[T:Ordering](maxSize:Int)(xs:T*): Beam[T] = new Beam[T](maxSize) ++= xs
}
