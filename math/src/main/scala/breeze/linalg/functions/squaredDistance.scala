package breeze.linalg

import breeze.generic.UFunc
import breeze.generic.UFunc.UImpl2

/**
 * Computes the squared distance between two vectors.
 *
 * @author dlwh
 **/
object squaredDistance extends UFunc {

  implicit def squaredDistanceFromZippedValues[T, U](implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] = {
    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        var squaredDistance = 0.0
        zipValues(v, v2).foreach { (a, b) =>
          val score = a - b
          squaredDistance += (score * score)
        }
        squaredDistance
      }
    }
  }
}
