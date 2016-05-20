package breeze.linalg.support

import breeze.linalg.Window

trait CanTraverseWindow[From, WindowType] {
  def apply[A](from: From, window: Window)(f: WindowType => A)
}
