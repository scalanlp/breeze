package breeze.linalg

import breeze.generic.UFunc.{InPlaceImpl, InPlaceImpl2, UImpl, UImpl2}
import breeze.linalg.support.{CanCollapseWindow, CanForeachValues, CanIterateWindow, CanMapValues, CanTraverseWindow, ScalarOf}
import breeze.math.Semiring
import breeze.storage.Zero

import scala.collection.AbstractIterator
import scala.reflect.ClassTag

case class WindowedDenseVector[@specialized(Double, Int, Float, Long) V](underlying: DenseVector[V], window: Window)
  extends WindowedLike[DenseVector[V], DenseVector[V], WindowedDenseVector[V]]

case class WindowedDenseVectorIterator[@specialized(Double, Int, Float, Long) V : ClassTag : Semiring](from: DenseVector[V], window: Window)
  extends AbstractIterator[DenseVector[V]] with Iterator[DenseVector[V]] {

  val numOfChunks = ((from.length - window.length) / window.step) + 1
  var cursor = 0
  var start = 0

  override def length: Int = numOfChunks

  override def size: Int = numOfChunks

  def hasNext: Boolean = cursor != numOfChunks

  def next(): DenseVector[V] = {
    if (cursor >= numOfChunks)
      throw new NoSuchElementException()

    val end = start + window.length
    val ret = if (end < from.length) from.slice(start, end) else pad(from.slice(start, from.length - 1))

    // update position of cursor and start
    cursor = cursor + 1
    start = start + window.step

    ret
  }

  private def pad(chunk: DenseVector[V]) : DenseVector[V] = window match {
    case PaddedWindow(length, _, padMode) => pad(chunk, length, padMode)
    case _ => chunk
  }

  private def pad(chunk: DenseVector[V], length: Int, padMode: Options.OptPadMode) : DenseVector[V] = {
    chunk
    //        I don't know why this isn't working but for some reason scala cannot find the implicit for CanPadRight[DenseVector[V]]
    //
    //        import CanPadRight._
    //        padRight[V](chunk, dimensions = window.length, mode = padMode)
  }
}

object WindowedDenseVector {

  implicit def scalarOf[V]: ScalarOf[WindowedDenseVector[V], DenseVector[V]] = ScalarOf.dummy

  implicit def canIterateWindow[V : ClassTag : Semiring] = new CanIterateWindow[DenseVector[V], DenseVector[V]] {
    def apply[A](from: DenseVector[V], window: Window): Iterator[DenseVector[V]] = WindowedDenseVectorIterator[V](from, window)
  }

  implicit def canTraverseWindow[V](implicit canIterateWindow: CanIterateWindow[DenseVector[V], DenseVector[V]]) =
    new CanTraverseWindow[DenseVector[V], DenseVector[V]] {
      def apply[A](from: DenseVector[V], window: Window)(f: (DenseVector[V]) => A) = canIterateWindow(from, window).foreach(f)
    }

  implicit def canCollapseWindow[V : ClassTag : Semiring, CollapsedType : ClassTag : Zero] :
   CanCollapseWindow[DenseVector[V], DenseVector[V], CollapsedType, DenseVector[CollapsedType]] =
    new CanCollapseWindow[DenseVector[V], DenseVector[V], CollapsedType, DenseVector[CollapsedType]] {
      def apply(from: DenseVector[V], window: Window)(f: (DenseVector[V]) => CollapsedType) : DenseVector[CollapsedType] = {
        new DenseVector[CollapsedType](WindowedDenseVectorIterator[V](from, window).map(f).toArray)
      }
    }

  implicit def canMapValues[V, CollapsedType, ResultType](implicit canCollapseWindow: CanCollapseWindow[DenseVector[V], DenseVector[V], CollapsedType, ResultType]) =
    new CanMapValues[WindowedDenseVector[V], DenseVector[V], CollapsedType, ResultType] {
      def apply(from: WindowedDenseVector[V], f: (DenseVector[V]) => CollapsedType) = canCollapseWindow(from.underlying, from.window)(f)
    }

  implicit def canForeach[V](implicit canTraverseWindow: CanTraverseWindow[DenseVector[V], DenseVector[V]]) =
    new CanForeachValues[WindowedDenseVector[V], DenseVector[V]] {
      def foreach[U](from: WindowedDenseVector[V], f: (DenseVector[V]) => U) = canTraverseWindow(from.underlying, from.window)(f)
    }

  implicit def windowOp[Op, T,CollapsedType, ResultType](implicit handhold: CanCollapseWindow.HandHold[DenseVector[T], DenseVector[T]],
                                                         canCollapseWindow: CanCollapseWindow[DenseVector[T], DenseVector[T], CollapsedType, ResultType],
                                                         op: UImpl[Op, DenseVector[T], CollapsedType]) =
    new UImpl[Op, WindowedDenseVector[T], ResultType] {
      def apply(v: WindowedDenseVector[T]): ResultType = {
        canCollapseWindow(v.underlying, v.window){
          op(_)
        }
      }
    }

  implicit def windowOp2[Op, T, ArgumentType, CollapsedType, ResultType](implicit handhold: CanCollapseWindow.HandHold[DenseVector[T], DenseVector[T]],
                                                                         canCollapseWindow: CanCollapseWindow[DenseVector[T], DenseVector[T], CollapsedType, ResultType],
                                                                         op: UImpl2[Op, DenseVector[T], ArgumentType, CollapsedType]) =
    new UImpl2[Op, WindowedDenseVector[T], ArgumentType, ResultType] {
      def apply(windowed: WindowedDenseVector[T], v2: ArgumentType): ResultType = {
        canCollapseWindow(windowed.underlying, windowed.window) {
          op(_, v2)
        }
      }
    }

  implicit def windowInplaceOp[Op, T](implicit canTraverseWindow: CanTraverseWindow[DenseVector[T], DenseVector[T]],
                                      op: InPlaceImpl[Op, DenseVector[T]]) =
    new InPlaceImpl[Op, WindowedDenseVector[T]] {
      def apply(windowed: WindowedDenseVector[T]): Unit = {
        canTraverseWindow(windowed.underlying, windowed.window) {
          op(_)
        }
      }
    }

  implicit def windowInplaceOp2[Op, T, RHS, Result](implicit canTraverseWindow: CanTraverseWindow[DenseVector[T], DenseVector[T]],
                                                    op: InPlaceImpl2[Op, DenseVector[T], RHS]) =
    new InPlaceImpl2[Op, WindowedDenseVector[T], RHS] {
      def apply(v: WindowedDenseVector[T], v2: RHS) {
        canTraverseWindow(v.underlying, v.window) {
          op(_, v2)
        }
      }
    }
}
