package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.{ZippedValues, zipValues}
import breeze.numerics._

/**
 * breeze
 * 9/2/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 */

trait DistanceMeasure extends UFunc {
}

object distance extends UFunc {
  implicit def genericDistance[DistanceType, T, U](implicit dist: DistanceType <:< DistanceMeasure,
                                                   dm: UFunc.UImpl2[DistanceType,T,U,Double]): Impl3[DistanceType,T,U,Double] =
    new Impl3[DistanceType, T, U, Double] {
      def apply(v: DistanceType, v2: T, v3: U): Double = v.apply(v2,v3)(dm)
    }
}

/**
 * A Euclidean distance metric implementation between two points
 */
object euclideanDistance extends DistanceMeasure {
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

/**
 * A Manhattan distance measure implementation between two points
 */
object manhattanDistance extends DistanceMeasure {

  implicit def manhattanDistanceFromZippedValues[T, U]
  (implicit zipImpl: zipValues.Impl2[T, U, ZippedValues[Double, Double]]): Impl2[T, U, Double] = {
    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        var distance = 0.0
        zipValues(v, v2).foreach{ (a, b) =>
          distance += abs(a - b)
        }
        distance
      }
    }
  }
}

/**
 * Computes the squared distance between two vectors.
 *
 * @author dlwh
 **/
object squaredDistance extends UFunc with DistanceMeasure {

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


object tanimotoDistance extends UFunc with DistanceMeasure {

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

/**
 * A Minkowski distance metric implementation between two points
 */
object minkowskiDistance extends UFunc with DistanceMeasure {

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
