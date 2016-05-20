package breeze.linalg.support

import breeze.linalg.Window

trait CanCollapseWindow[From, WindowType, CollapsedType, ResultType] {
  def apply(from: From, window: Window)(f: WindowType => CollapsedType): ResultType
}

object CanCollapseWindow {
  class HandHold[From, WindowType]
}
