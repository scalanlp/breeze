package breeze

import breeze.macros.{expandArgs, expand}

/**
 * TODO
 *
 * @author dlwh
 **/
class DVVV {
  @expand
  def foo[@expandArgs(Int, Double) T] = {
    val x: T = 3.0
    val y: Int = 3
    x

  }
}
