package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.linalg._
import breeze.numerics.abs

/**
 * A Chebyshev distance metric implementation between two points
 */
object chebyshevDistance extends UFunc {

  implicit def chebyshevDistanceFromZippedValues[T, U]
      (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] = {

    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        var max = Double.MinValue

        zipValues(v, v2).foreach { (a, b) =>
          if(abs(a - b) > max) {
            max = abs(a - b)
          }
        }
        max
      }
    }
  }
}
