package breeze.linalg

import breeze.linalg.Options.OptPadMode
import breeze.math.Semiring

import scala.reflect.ClassTag

case class WindowedVector[VectorType, WindowType](underlying: VectorType, window: Window)
  extends WindowedLike[VectorType, WindowType, WindowedVector[VectorType, WindowType]] with Windowed[VectorType, WindowType]

object WindowedVector
  extends LowPriorityWindowed[WindowedVector] with WindowedOps[WindowedVector] {

  def apply[T](underlying: DenseVector[T], length: Int) : WindowedVector[DenseVector[T], DenseVector[T]] = {
    apply(underlying, StandardWindow(length))
  }

  def apply[T](underlying: DenseVector[T], length: Int, padMode: OptPadMode): WindowedVector[DenseVector[T], DenseVector[T]] = {
    this(underlying, PaddedWindow(length, padMode = padMode))
  }
}

/**
  * Base class for iterating over a windowed vector. Handles both padded and standard windows
  */
abstract class WindowedVectorIterator[ElementType,
                                      VectorType <: Vector[ElementType],
                                      WindowType <: Vector[ElementType]](underlying: VectorType, window: Window)
  extends Iterator[WindowType] {

  val numOfChunks = ((underlying.length - window.length) / window.step) + 1
  var cursor = 0
  var start = 0

  protected def padResult(chunk: WindowType, length: Int, padMode: Options.OptPadMode) : WindowType

  protected def sliceUnderlying(start: Int, end: Int) : WindowType

  override def length: Int = numOfChunks

  override def size: Int = numOfChunks

  def hasNext: Boolean = cursor != numOfChunks

  def next(): WindowType = {
    if (cursor >= numOfChunks)
      throw new NoSuchElementException()

    val end = start + window.length
    val ret = if (end < underlying.length)
      sliceUnderlying(start, end)
    else
      padResult(sliceUnderlying(start, underlying.length - 1))

    // update position of cursor and start
    cursor = cursor + 1
    start = start + window.step

    ret
  }

  private def padResult(chunk: WindowType) : WindowType = window match {
    case PaddedWindow(length, _, padMode) => padResult(chunk, length, padMode)
    case _ => chunk
  }
}

/**
  * DenseVector implementation of windowed vector iterator
  */
private case class WindowedDenseVectorIterator[T: ClassTag : Semiring](underlying: DenseVector[T], window: Window)
  extends WindowedVectorIterator[T, DenseVector[T], DenseVector[T]](underlying, window) {

  def sliceUnderlying(start: Int, end: Int): DenseVector[T] = underlying.slice(start, end)

  def padResult(chunk: DenseVector[T], length: Int, padMode: OptPadMode): DenseVector[T] = {
    //    this doesn't work as scala complains it can't find the canPad implicit, any idea???
    //    import CanPadRight._
    //    import Options._
    //
    //    padRight[T](chunk, dimensions = window.length, mode = padMode)

    chunk
  }
}

