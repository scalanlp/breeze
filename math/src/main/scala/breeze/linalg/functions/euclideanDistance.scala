package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.linalg._

/**
 * A Euclidean distance metric implementation between two points
 */
object euclideanDistance extends UFunc {

  implicit def euclideanDistanceFromZippedValues[T, U]
      (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] = {

    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        var distance = 0.0

        zipValues(v, v2).foreach { (a, b) =>
          val score = a - b
          distance += (score * score)
        }
        Math.sqrt(distance)
      }
    }
  }
}
