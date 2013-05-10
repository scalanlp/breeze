package breeze.optimize

import breeze.optimize.FirstOrderMinimizer.OptParams

/**
 *
 * @author dlwh
 */
trait FirstOrderOption extends (OptParams => OptParams) {
}

object FirstOrderOption {
  implicit def intToFirstOrderOption(x: Int) = new MaxIterations(3)
}

case class MaxIterations(num: Int) extends  {
  def apply(params: OptParams):OptParams = {
    params.copy(maxIterations = 1)
  }
}

trait HasMaxIterations[Self] {
  def setMaxIterations(iter: Int):Self
}