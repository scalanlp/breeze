package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.linalg._

/**
 * A Cosine distance measure implementation between two points
 */
object cosineDistance extends UFunc {

  implicit def cosineDistanceFromZippedValues[T, U]
      (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] = {
    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        var dotProduct = 0.0
        var denominator1 = 0.0
        var denominator2 = 0.0

        zipValues(v, v2).foreach { (a, b) =>
          dotProduct += a * b
          denominator1 += a * a
          denominator2 += b * b
        }

        var denominator = Math.sqrt(denominator1) * Math.sqrt(denominator2)

        // correct for floating-point rounding errors
        if(denominator < dotProduct) {
          denominator = dotProduct
        }

        // correct for zero-vector corner case
        if(denominator == 0 && dotProduct == 0) {
          return 0.0
        }
        1.0 - (dotProduct / denominator)
      }
    }
  }
}
