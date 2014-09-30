package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.linalg._

/**
 * A Minkowski distance metric implementation between two points
 */
object minkowskiDistance extends UFunc {

  implicit def minkowskiDistanceFromZippedValues[T, U]
      (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl3[T, U, Double, Double] = {

    new Impl3[T, U, Double, Double] {
      def apply(v: T, v2: U, exponent: Double): Double = {
        var cum = 0.0
        zipValues(v, v2).foreach {(a, b) =>
          cum += Math.pow(a - b, exponent)
        }
        Math.pow(cum, 1 / exponent)
      }
    }
  }
}
