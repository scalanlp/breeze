package breeze.collection.mutable

import breeze.util.ReflectionUtil

import scala.collection.generic._
import scala.collection.{IndexedSeqOps => _, _}
import scala.collection.mutable._
import scala.reflect.ClassTag

// https://en.wikipedia.org/wiki/Circular_buffer
class RingBuffer[A](val capacity: Int)
  extends AbstractBuffer[A]
    with IndexedBuffer[A]
    with IndexedSeqOps[A, RingBuffer, RingBuffer[A]]
    with StrictOptimizedSeqOps[A, RingBuffer, RingBuffer[A]]
    with DefaultSerializable
    with Builder[A, scala.Seq[A]] {
  private val buf = ArrayBuffer.fill[A](capacity)(null.asInstanceOf[A])

  // if the buffer is full, we set endPos = -1
  def isFull: Boolean = endPos < 0

  override def knownSize = length

  private var startPos = 0
  private var endPos = 0
  private def trueEnd = if (isFull) startPos else endPos

  override def apply(n: Int): A = buf(index(n))

  def length: Int = {
    if (isFull) capacity else if (endPos < startPos) capacity + endPos - startPos else endPos - startPos
  }

  private def index(i: Int) = {
    boundsCheck(i)
    (i + startPos) % capacity
  }

  private def boundsCheck(i: Int, inclusiveEnd: Boolean = false): Unit = {
    if (i < 0 || i > length || (!inclusiveEnd && i == length))
      throw new IndexOutOfBoundsException(s"$i out of bounds for RingBuffer with length $length and capacity $capacity")
  }

  def update(n: Int, x: A): Unit = buf(index(n)) = x

  private def advance(pos: Int) = (pos + 1) % capacity
  private def recede(pos: Int) = if (pos - 1 < 0) capacity - 1 else pos - 1

  def addOne(x: A): this.type = {
    if (isFull) {
      buf(startPos) = x
      startPos = advance(startPos)
    } else {
      buf(endPos) = x
      endPos = advance(endPos)
      if (endPos == startPos) {
        endPos = -1
      }
    }
    this
  }

  def clear(): Unit = {
    startPos = 0; endPos = 0
  }

  override def prepend(elem: A): this.type = {
    startPos = recede(startPos)
    buf(startPos) = elem
    if (endPos == startPos)
      endPos = -1
    this
  }

  /**
   * Note that we treat the semantics of this operation as "truncate to length n,
   * add elems, then add the 'old' elements after n," erasing elements as necessary.
   * Note that it's entirely possible that some or even all of the inserted elements
   * will be overwritten by current elements.
    **/
  override def insertAll(n: Int, elems: scala.IterableOnce[A]): Unit = {
    if (n == length) {
      this ++= elems
    } else {
      boundsCheck(n)
      // this is for the ones that we have to shift in later
      val toInsertAfter = slice(n, length)
      dropRightInPlace(length - n)
      this ++= elems
      this ++= toInsertAfter
    }
  }

  def insert(idx: Int, elem: A): Unit = insertAll(idx, Iterator.single(elem))

  override def remove(n: Int, count: Int): Unit = {
    boundsCheck(n, inclusiveEnd = true)
    boundsCheck(n + count, inclusiveEnd = true)
    if (count == 0) {
      // nothing
    } else if (n + count == length) {
      endPos = (trueEnd + capacity - count) % capacity
      assert(length == n)
    } else if (n == 0) {
      if (isFull) {
        // no longer full
        endPos = startPos
      }
      startPos += count
      startPos %= capacity
    } else {
      // this case is trickier, since we're removing from the middle
      // we punt on doing this "well" and just get it done
      val elements = to(ArrayBuffer)
      elements.remove(n, count)
      clear()
      this ++= elements
    }
  }

  def remove(n: Int): A = {
    val v = apply(n)
    remove(n, 1)
    v
  }

  override def subtractOne(elem: A): this.type = {
    val pos = indexOf(elem)
    if (pos >= 0) {
      remove(pos)
    }
    this
  }

  override def clone(): RingBuffer[A] = new RingBuffer[A](capacity) ++= this
  override def className: String = "RingBuffer"

  override def iterator: Iterator[A] = Iterator.range(0, length).map(apply)

  /** returns a string representing the buffer's current internal state. Begin is marked with backtick and end with ' */
  def stateString: String = {
    val out = new StringBuilder("RingBuffer(")

    for (i <- 0 until capacity) {
      if (i != 0)
        out ++= ", "

      if (i == startPos && startPos == endPos) {
        out ++= "`'"
      } else {
        if (i == trueEnd) {
          out ++= "'"
        }

        if (i == startPos) {
          out ++= "`"
        }

        out ++= buf(i).toString
      }
    }

    out ++= ")"

    out.toString
  }

  override def result(): scala.Seq[A] = iterator.toIndexedSeq

  override protected def fromSpecific(coll: IterableOnce[A]): RingBuffer[A] = {
    new RingBuffer(this.capacity) ++= coll.iterator.to(ArrayBuffer)
  }

  override protected def newSpecificBuilder: Builder[A, RingBuffer[A]] = {
    new GrowableBuilder(new RingBuffer[A](capacity))
  }

  override def empty: RingBuffer[A] = new RingBuffer(capacity)

  override def iterableFactory: SeqFactory[RingBuffer] = new SeqFactory[RingBuffer] {
    override def newBuilder[T]: mutable.Builder[T, RingBuffer[T]] = RingBuffer.canBuildFrom[A, T].newBuilder(RingBuffer.this)

    override def empty[A]: RingBuffer[A] = newBuilder[A].result()
    override def from[T](f: IterableOnce[T]): RingBuffer[T] = (newBuilder[T] ++= f).result()
  }
}

object RingBuffer {
  def apply[A](capacity: Int)(elems: A*): RingBuffer[A] = new RingBuffer[A](capacity) ++= elems

  implicit def canBuildFrom[A, B]: BuildFrom[RingBuffer[A], B, RingBuffer[B]] = {
    new BuildFrom[RingBuffer[A], B, RingBuffer[B]] {
      override def newBuilder(from: RingBuffer[A]): mutable.Builder[B, RingBuffer[B]] = {
        new GrowableBuilder(new RingBuffer[B](from.capacity))
      }

      override def fromSpecific(from: RingBuffer[A])(it: IterableOnce[B]): RingBuffer[B] = {
        (newBuilder(from) ++= it).result()
      }
    }
  }
}
