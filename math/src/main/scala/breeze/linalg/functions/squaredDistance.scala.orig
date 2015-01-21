package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.operators.{OpMulInner, OpSub}

/**
 * Computes the squared distance between two vectors.
 *
 * @author dlwh
 **/
object squaredDistance extends UFunc {


  implicit def distanceFromDotAndSub[T, U, V]
  (implicit subImpl: OpSub.Impl2[T, U, V],
   dotImpl: OpMulInner.Impl2[V, V, Double]): Impl2[T, U, Double] = {

    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        val diff = subImpl(v, v2)
        dotImpl(diff, diff)
      }
    }
  }


}
