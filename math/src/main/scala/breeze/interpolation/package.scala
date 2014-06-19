package breeze

import breeze.generic.{VariableUFunc, MappingUFunc, UFunc}
import scala.reflect.ClassTag
import breeze.math.Field
import breeze.linalg.Vector

package object interpolation {

  object UnivariateInterpolatorImpl extends UFunc with MappingUFunc {
    implicit def impl[T]: Impl2[UnivariateInterpolator[T], T, T] = new Impl2[UnivariateInterpolator[T], T, T] {
      def apply(k: UnivariateInterpolator[T], v: T): T = k(v)
    }
  }

  trait UnivariateInterpolator[T] extends VariableUFunc[UnivariateInterpolatorImpl.type, UnivariateInterpolator[T]] {
    def apply(x: T): T
  }

  abstract class HandyUnivariateInterpolator[T:ClassTag:Field:Ordering]
      (x_coords: Vector[T],
       y_coords: Vector[T])
      extends UnivariateInterpolator[T] {

    if (x_coords.size != x_coords.toArray.toSet.size)
      throw new Exception("x coordinates must be unique")
    if (x_coords.size != y_coords.size)
      throw new Exception("x_coords and y_coords must be of the same size")
    if (x_coords.size == 0)
      throw new Exception("need to provide at least one pair of coordinates")

    private val nodes = x_coords.toArray zip y_coords.toArray sortBy {n => n._1}
    protected val X: Array[T] = nodes map {n => n._1}
    protected val Y: Array[T] = nodes map {n => n._2}

    private val ord = implicitly[Ordering[T]]
    import ord.mkOrderingOps

    def apply(x: T): T = {
      if (x < X(0) || x > X(X.size - 1))
        extrapolate(x)
      else
        interpolate(x)
    }

    protected def interpolate(x: T): T

    protected def extrapolate(x: T): T = {
      throw new IndexOutOfBoundsException("Out of the domain [" + X(0) + "," + X(X.size-1) + "]")
    }
  }

}
