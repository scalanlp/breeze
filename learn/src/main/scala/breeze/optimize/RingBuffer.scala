package breeze.optimize

import scala.collection.mutable._
import scala.collection.generic._

class RingBuffer[A](m: Int) extends Buffer[A] with GenericTraversableTemplate[A, RingBuffer] with BufferLike[A, RingBuffer[A]] with Builder[A, List[A]] {
  val buf = new ListBuffer[A]

  private def resize: Unit = while (buf.size > m) buf.remove(0)

  override def length = buf.length
  override def apply(n: Int): A = buf.apply(n)
  def update(n: Int, x: A) = buf.update(n, x)
  def +=(x: A): this.type = { buf.+=(x); resize; this }
  def clear() = buf.clear();
  def +=:(x: A): this.type = { buf.+=:(x); resize; this }
  def insertAll(n: Int, seq: scala.collection.Traversable[A]): Unit = buf.insertAll(n, seq)
  override def remove(n: Int, count: Int) = buf.remove(n, count)
  def result: List[A] = buf.result
  override def toList: List[A] = buf.toList
  def prependToList(xs: List[A]): List[A] = buf.prependToList(xs)
  def remove(n: Int): A = buf.remove(n)
  override def -=(elem: A): this.type = { buf.-=(elem); this }
  override def iterator = buf.iterator
  override def readOnly: List[A] = buf.readOnly
  override def equals(that: Any): Boolean = buf.equals(that)
  override def clone(): RingBuffer[A] = new RingBuffer(m) ++= this
  override def stringPrefix: String = "RingBuffer"
  override def companion: GenericCompanion[RingBuffer] = RingBuffer
}

object RingBuffer extends SeqFactory[RingBuffer] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, RingBuffer[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, RingBuffer[A]] = new GrowingBuilder(new RingBuffer[A](Int.MaxValue))
}