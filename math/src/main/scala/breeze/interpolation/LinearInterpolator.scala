package breeze.interpolation

/**
 *
 * @author chrismedrela
 */

import scala.reflect.ClassTag

import breeze.generic.{UFunc, MappingUFunc, VariableUFunc}
import breeze.linalg._
import breeze.linalg.support.CanMapValues
import breeze.math.Field

/*
trait UnivariateInterpolator[T] {
  def apply(x: T): T
  def applyCol[ColIn, ColOut](x: ColIn)(implicit cmv: CanMapValues[ColIn, T, T, ColOut]) = cmv.map(x, apply _)
}

abstract class HandyUnivariateInterpolator[T:Ordering:ClassTag:Field]
    (x_coords: Vector[T],
     y_coords: Vector[T])
     extends UnivariateInterpolator[T] {//extends UnivariateInterpolator{

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
      throw new Exception("Out of the domain")

    valueAt(x)
  }

  protected def valueAt(x: T): T
}


class LinearInterpolator[T:Ordering:ClassTag:Field]
    (x_coords: Vector[T],
     y_coords: Vector[T])
    extends HandyUnivariateInterpolator[T](x_coords, y_coords) {

  private val ord = implicitly[Ordering[T]]
  import ord.mkOrderingOps

  override protected def valueAt(x: T): T = {
    def bisearch(low: Int, high: Int): Int = (low+high)/2 match {
      case mid if low == high => mid
      case mid if X(mid) < x => bisearch(mid+1, high)
      case mid => bisearch(low, mid)
    }
    val index = bisearch(0, X.length-1)

    val f = implicitly[Field[T]]
    // w = (x - x1) / (x2 - x1)
    val w = f./(f.-(x,
                    X(index-1)),
                f.-(X(index),
                    X(index-1)))
    // u = 1 - w
    val u = f.-(f.one, w)

    // result = y1 * u + y2 * w
    f.+(f.*(Y(index-1), u),
        f.*(Y(index), w))
  }
}
*/

object UnivariateInterpolatorImpl extends UFunc with MappingUFunc {
  implicit object impl extends Impl2[UnivariateInterpolator,Double,Double] {
    def apply(k: UnivariateInterpolator, v: Double) = k(v)
  }
}

trait UnivariateInterpolator extends VariableUFunc[UnivariateInterpolatorImpl.type, UnivariateInterpolator] {
  def apply(x: Double): Double
}

abstract class HandyUnivariateInterpolator
    (x_coords: Vector[Double],
     y_coords: Vector[Double])
    extends UnivariateInterpolator {

  if (x_coords.size != x_coords.toArray.toSet.size)
    throw new Exception("x coordinates must be unique")
  if (x_coords.size != y_coords.size)
    throw new Exception("x_coords and y_coords must be of the same size")
  if (x_coords.size == 0)
    throw new Exception("need to provide at least one pair of coordinates")

  private val nodes = x_coords.toArray zip y_coords.toArray sortBy {n => n._1}
  protected val X: Array[Double] = nodes map {n => n._1}
  protected val Y: Array[Double] = nodes map {n => n._2}

  private val ord = implicitly[Ordering[Double]]
  import ord.mkOrderingOps

  def apply(x: Double): Double = {
    if (x < X(0) || x > X(X.size - 1))
      throw new IndexOutOfBoundsException("Out of the domain [" + X(0) + "," + X(X.size-1) + "]")

    valueAt(x)
  }

  protected def valueAt(b: Double): Double
}

class LinearInterpolator
    (x_coords: Vector[Double],
     y_coords: Vector[Double])
    extends HandyUnivariateInterpolator(x_coords, y_coords) {

  private val ord = implicitly[Ordering[Double]]
  import ord.mkOrderingOps

  override protected def valueAt(x: Double): Double = {
    def bisearch(low: Int, high: Int): Int = (low+high)/2 match {
      case mid if low == high => mid
      case mid if X(mid) < x => bisearch(mid+1, high)
      case mid => bisearch(low, mid)
    }
    val index = bisearch(0, X.length-1)

    val f = implicitly[Field[Double]]
    // w = (x - x1) / (x2 - x1)
    val w = f./(f.-(x,
                    X(index-1)),
                f.-(X(index),
                    X(index-1)))
    // u = 1 - w
    val u = f.-(f.one, w)

    // result = y1 * u + y2 * w
    f.+(f.*(Y(index-1), u),
        f.*(Y(index), w))
  }
}

/*
class MyInterpolatorUFuncWrapper2(val a: Double) extends UnivariateInterpolator {
  def valueAt(b: Double) = a + b + 1
}
*/