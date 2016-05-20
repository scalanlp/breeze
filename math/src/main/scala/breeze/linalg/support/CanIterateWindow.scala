package breeze.linalg.support

import breeze.linalg.Window


/**
  * @author Michael Petnuch
  * @version $Id$
  */
trait CanIterateWindow[From, WindowType] {
  def apply[A](from: From, window: Window) : Iterator[WindowType]
}
