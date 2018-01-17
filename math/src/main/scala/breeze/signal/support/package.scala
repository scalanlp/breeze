package breeze.signal

import breeze.numerics.{log, ceil, pow}

/**
 * @author ktakagaki
 * @date 2/3/14.
 */
package object support {

  /** Gives the next largest power of base. nextPower(x, 2) is equivalent to MATLAB nextPow2(x).
   */
  def nextPower(x: Double, base: Int): Double = pow(2d, ceil(log(x) / log(base.toDouble)))

  def nextPower2(x: Double): Double = nextPower(x, 2)

}
