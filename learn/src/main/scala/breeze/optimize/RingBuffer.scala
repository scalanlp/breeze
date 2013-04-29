package breeze.optimize

class RingBuffer[A](val index: Int, val data: Seq[A] = Nil) extends IndexedSeq[A] {
  def shiftLeft = new RingBuffer((index + 1) % data.size, data)
  def shiftRight = new RingBuffer((index + data.size - 1) % data.size, data)
  def length = data.length
  def apply(i: Int) = data((index + i) % data.size)
  def +(a: A) = new RingBuffer[A](index, data :+ a)
}