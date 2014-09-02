package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.linalg._

object tanimotoDistance extends UFunc {

  implicit def tanimotoDistanceFromZippedValues[T, U]
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

        var denominator = denominator1 + denominator2 - dotProduct

        // correct for fp round-off: distance >= 0
        if (denominator < dotProduct) {
          denominator = dotProduct
        }

        if (denominator > 0) {
          // denominator == 0 only when dot(a,a) == dot(b,b) == dot(a,b) == 0
          return 1.0 - dotProduct / denominator
        } else {
          return 0.0
        }
      }
    }
  }
}

