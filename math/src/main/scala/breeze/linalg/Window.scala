package breeze.linalg

import breeze.linalg.Options.OptPadMode
import breeze.linalg.support.{CanCollapseAxis, CanCollapseWindow, CanSlice, CanSlice2}

sealed trait Window {
  def length: Int
  def step: Int
}

case class StandardWindow(length: Int, step: Int = 1) extends Window {
  require(length >= 1 && step >= 1, "length=%d and step=%d, but both must be positive".format(length, step))
}

case class PaddedWindow(length: Int, step: Int = 1, padMode: OptPadMode = Options.Zero) extends Window {
  require(length >= 1 && step >= 1, "length=%d and step=%d, but both must be positive".format(length, step))
}
