package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.operators.OpMulInner

/**
 * The cosine distance between two points: cosineDistance(a,b) = 1 - (a dot b)/(norm(a) * norm(b))
 */
object cosineDistance extends UFunc {
  implicit def cosineDistanceFromDotProductAndNorm[T, U](
                                                          implicit dot: OpMulInner.Impl2[T, U, Double],
                                                          normT: norm.Impl[T, Double],
                                                          normU: norm.Impl[U, Double]): Impl2[T, U, Double] = {
    new Impl2[T, U, Double] {
      override def apply(v: T, v2: U): Double = {
        val denom = norm(v) * norm(v2)
        val dotProduct = dot(v, v2)
        if (denom == 0.0) {
          0.0
        } else {
          1 - dotProduct / denom
        }
      }
    }
  }

  implicit def cosineDistanceFromDotProductAndNorm_F[T, U](
                                                            implicit dot: OpMulInner.Impl2[T, U, Float],
                                                            normT: norm.Impl[T, Double],
                                                            normU: norm.Impl[U, Double]): Impl2[T, U, Double] = {
    new Impl2[T, U, Double] {
      override def apply(v: T, v2: U): Double = {
        val denom = norm(v) * norm(v2)
        val dotProduct = dot(v, v2)
        if (denom == 0.0) {
          0.0
        } else {
          1 - dotProduct / denom
        }
      }
    }


  }
}
