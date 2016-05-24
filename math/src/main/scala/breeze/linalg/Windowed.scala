package breeze.linalg

import breeze.generic.UFunc.{InPlaceImpl, InPlaceImpl2, UImpl, UImpl2}
import breeze.linalg.support.{CanCollapseWindow, CanForeachValues, CanIterateWindow, CanMapValues, CanTraverseWindow, ScalarOf}

import scala.language.higherKinds

/**
  * Base trait for objects which have been windowed.
  */
trait Windowed[+T, B] extends NumericOps[Windowed[T, B]] {
  def window: Window

  def underlying: T
}

/**
  * A template trait for windowed objects of type `Window[UnderlyingType, WindowType]`
  *
  * A windowed object can be thought of as a sequence of views on the underlying type as defined by their window.
  */
trait WindowedLike[UnderlyingType, WindowType, Self <: Windowed[UnderlyingType, WindowType]]
  extends Windowed[UnderlyingType, WindowType] with NumericOps[Self] {

  def repr: Self = this.asInstanceOf[Self]

  def map[U, ResultType](f: WindowType => U)(implicit canMapValues: CanMapValues[Self, WindowType, U, ResultType]): ResultType = {
    canMapValues(repr, f)
  }

  def iterator(implicit canIterateWindow: CanIterateWindow[UnderlyingType, WindowType]) : Iterator[WindowType] = {
    canIterateWindow(repr.underlying, repr.window)
  }

  def foreach[U](f: WindowType => U)(implicit canForeachValues: CanForeachValues[Self, WindowType]): Unit = {
    canForeachValues.foreach(repr, f)
  }

  def foldLeft[B](z: B)(f: (B, WindowType) => B)(implicit canTraverseWindow: CanTraverseWindow[UnderlyingType, WindowType]) : B = {
    var acc = z
    canTraverseWindow(repr.underlying, repr.window) {
      c => acc = f(acc, c)
    }

    acc
  }
}

/**
  * A template trait for companion objects of Windowed which provide implicits for windowing UFuncs
  */
trait WindowedOps[CC[T, B] <: Windowed[T, B]] {
  implicit def windowedOp[Op, UnderlyingType, WindowedType, CollapsedType, ResultType]
  (implicit handhold: CanCollapseWindow.HandHold[UnderlyingType, WindowedType],
   canCollapseWindow: CanCollapseWindow[UnderlyingType, WindowedType, CollapsedType, ResultType],
   op: UImpl[Op, WindowedType, CollapsedType]) = {
    new UImpl[Op, CC[UnderlyingType, WindowedType], ResultType] {
      def apply(v: CC[UnderlyingType, WindowedType]): ResultType = {
        canCollapseWindow(v.underlying, v.window) {
          op(_)
        }
      }
    }
  }

  implicit def windowedOp2[Op, UnderlyingType, WindowType, ArgumentType, CollapsedType, ResultType]
  (implicit handhold: CanCollapseWindow.HandHold[WindowType, WindowType],
   canCollapseWindow: CanCollapseWindow[UnderlyingType, WindowType, CollapsedType, ResultType],
   op: UImpl2[Op, WindowType, ArgumentType, CollapsedType]) = {
    new UImpl2[Op, CC[UnderlyingType, WindowType], ArgumentType, ResultType] {
      def apply(windowed: CC[UnderlyingType, WindowType], argument: ArgumentType): ResultType = {
        canCollapseWindow(windowed.underlying, windowed.window) {
          op(_, argument)
        }
      }
    }
  }

  implicit def windowedInplaceOp[Op, UnderlyingType, WindowType]
  (implicit canTraverseWindow: CanTraverseWindow[UnderlyingType, WindowType],
   op: InPlaceImpl[Op, WindowType]) = {
    new InPlaceImpl[Op, CC[UnderlyingType, WindowType]] {
      def apply(windowed: CC[UnderlyingType, WindowType]): Unit = {
        canTraverseWindow(windowed.underlying, windowed.window) {
          op(_)
        }
      }
    }
  }

  implicit def windowedInplaceOp2[Op, UnderlyingType, WindowType, ArgumentType, ResultType]
  (implicit canTraverseWindow: CanTraverseWindow[UnderlyingType, WindowType],
                           op: InPlaceImpl2[Op, WindowType, ArgumentType]) = {
    new InPlaceImpl2[Op, CC[UnderlyingType, WindowType], ArgumentType] {
      def apply(v: CC[UnderlyingType, WindowType], argument: ArgumentType) {
        canTraverseWindow(v.underlying, v.window) {
          op(_, argument)
        }
      }
    }
  }
}

/**
  * A template trait for companion objects of Windowed which provide low priority implicits
  */
trait LowPriorityWindowed[CC[T, B] <: Windowed[T, B]] {
  implicit def scalarOf[UnderlyingType, WindowType]: ScalarOf[CC[UnderlyingType, WindowType], WindowType] = ScalarOf.dummy

  implicit def canMapValues[T, B, CollapsedType, ResultType]
  (implicit canCollapseWindow: CanCollapseWindow[T, B, CollapsedType, ResultType]): CanMapValues[CC[T, B], B, CollapsedType, ResultType] = {
    new CanMapValues[CC[T, B], B, CollapsedType, ResultType] {
      def apply(from: CC[T, B], f: (B) => CollapsedType): ResultType = {
        canCollapseWindow(from.underlying, from.window) {
          f
        }
      }
    }
  }

  implicit def canForeachValues[T, B](implicit canTraverseWindow: CanTraverseWindow[T, B]): CanForeachValues[CC[T,B], B] = {
    new CanForeachValues[CC[T, B], B] {
      def foreach[U](from: CC[T, B], f: (B) => U): Unit = {
        canTraverseWindow(from.underlying, from.window)(f)
      }
    }
  }
}
