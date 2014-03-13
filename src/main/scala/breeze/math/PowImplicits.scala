package breeze.math

import breeze.numerics.IntMath

/**
 * importing this gives numeric enables a "pow" method on basic numeric types
 *
 * @author dlwh
 **/
object PowImplicits {
  // just to make some unrolling less terrible
  // TODO: move this somewhere sensible

  implicit class FloatPow(x: Float) {
    def pow(y: Float) = math.pow(x,y).toFloat
  }
}
