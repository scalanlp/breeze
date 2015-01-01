package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.operators.OpSub
import breeze.numerics.pow

/**
 * A Minkowski distance metric implementation between two points
 */
object minkowskiDistance extends UFunc with minkowskiDistanceLowPrio {
  implicit def minkowskiDistanceFromSubtractionAndNorm[T, U, V]
  (implicit sub: OpSub.Impl2[T, U, V],
   powImpl: norm.Impl2[V, Double, V],
   sumImpl: sum.Impl[V, Double]): Impl3[T, U, Double, Double] = {

    new Impl3[T, U, Double, Double] {
      def apply(v: T, v2: U, exponent: Double): Double = {
        pow(sum(powImpl(sub(v, v2), exponent)), 1/exponent)
      }
    }
  }

}

sealed trait minkowskiDistanceLowPrio { this: minkowskiDistance.type =>

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
