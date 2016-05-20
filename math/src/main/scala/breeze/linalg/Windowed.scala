package breeze.linalg

import breeze.generic.UFunc.{InPlaceImpl, InPlaceImpl2, UImpl, UImpl2}
import breeze.linalg.support.{CanCollapseWindow, CanForeachValues, CanIterateWindow, CanMapValues, CanTraverseWindow, ScalarOf}

trait Windowed[+T, B] extends NumericOps[Windowed[T, B]] {
  def window: Window

  def underlying: T
}

trait WindowedLike[From, WindowType, Self <: Windowed[From, WindowType]] extends Windowed[From, WindowType] with NumericOps[Self] {
  def repr = this.asInstanceOf[Self]

  def map[U, Res](f: WindowType => U)(implicit canMapValues: CanMapValues[Self, WindowType, U, Res]) = canMapValues(repr, f)

  def foreach[U](f: WindowType => U)(implicit canForeachValues: CanForeachValues[Self, WindowType]) = canForeachValues.foreach(repr, f)

  def iterator(implicit canIterateWindow: CanIterateWindow[From, WindowType]) = canIterateWindow(underlying, window)
}
