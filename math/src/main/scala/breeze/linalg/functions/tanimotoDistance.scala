package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.linalg.operators.OpMulInner

object tanimotoDistance extends UFunc {
  implicit def tanimotoDistanceFromDotProduct[T, U](
      implicit dotTU: OpMulInner.Impl2[T, U, Double],
      dotTT: OpMulInner.Impl2[T, T, Double],
      dotUU: OpMulInner.Impl2[U, U, Double]): Impl2[T, U, Double] = {
    new Impl2[T, U, Double] {
      override def apply(v: T, v2: U): Double = {
        val dotProduct = dotTU(v, v2)
        val denom = dotTT(v, v) + dotUU(v2, v2) - dotProduct
        if (denom == 0.0) {
          0.0
        } else {
          1 - dotProduct / denom
        }
      }
    }
  }

}
