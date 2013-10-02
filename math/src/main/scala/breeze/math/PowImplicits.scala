package breeze.math

import breeze.numerics.IntMath

/**
 * importing this gives numeric types a "pow" method.
 *
 * @author dlwh
 **/
object PowImplicits {
  // just to make some unrolling less terrible
  // TODO: move this somewhere sensible
  implicit class DoublePow(x: Double) {
    def pow(y: Double) = math.pow(x,y)
  }

  implicit class FloatPow(x: Float) {
    def pow(y: Float) = math.pow(x,y).toFloat
  }

  implicit class IntPow(x: Int) {
    def pow(y: Int) = IntMath.ipow(x, y)
  }

  implicit class LongPow(x: Long) {
    def pow(y: Long) = IntMath.ipow(x, y)
  }

}
