package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand

/**
 * Created by dlwh on 11/1/15.
 */
object isClose extends UFunc {

  val DEFAULT_TOLERANCE = 1e-8

  implicit def defaultTolImpl[A, B](implicit impl3: Impl3[A, B, Double, Boolean]): Impl2[A, B, Boolean] = {
    new Impl2[A, B, Boolean] {
      override def apply(v: A, v2: B): Boolean = impl3(v, v2, DEFAULT_TOLERANCE)
    }
  }

  @expand
  implicit def impl[@expand.args(Double, Float) T]: Impl3[T, T, Double, Boolean] =
    new Impl3[T, T, Double, Boolean] {
      override def apply(v: T, v2: T, v3: Double): Boolean = math.abs(v - v2) <= v3
    }

  implicit def fromZipValues[A, B, V1, V2](implicit
      czv: zipValues.Impl2[A, B, ZippedValues[V1, V2]],
      base: Impl3[V1, V2, Double, Boolean]
  ): Impl3[A, B, Double, Boolean] = {
    new Impl3[A, B, Double, Boolean] {
      override def apply(a: A, b: B, tol: Double): Boolean = {
        czv(a, b).forall { (v1, v2) =>
          base(v1, v2, tol)
        }
      }
    }
  }

}
