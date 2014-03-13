package breeze.interpolation

/**
 *
 * @author chrismedrela
 */

import scala.annotation.tailrec
import scala.reflect.ClassTag

import breeze.linalg._
import breeze.linalg.support.CanMapValues
import breeze.math.Field

/*
trait UnivariateInterpolator {
    def apply(x: Double): Double
    def apply(x: Vector[Double]): Vector[Double] = x map apply
}

abstract class HandyUnivariateInterpolator
    (x_coords: Vector[Double],
     y_coords: Vector[Double])
    (implicit semiring: Semiring[E]) extends UnivariateInterpolator{
  if (x_coords.size != x_coords.toArray.toSet.size)
    throw new Exception("x coordinates must be unique")
  if (x_coords.size != y_coords.size)
    throw new Exception("x_coords and y_coords must be of the same size")
  if (x_coords.size == 0)
    throw new Exception("need to provide at least one pair of coordinates")

  private val nodes = x_coords.toArray zip y_coords.toArray sortBy {n => n._1}
  protected val X: Array[Double] = nodes map {n => n._1}
  protected val Y: Array[Double] = nodes map {n => n._2}

  override def apply(x: Double): Double = {
    if (x < X(0) || x > X(X.size - 1))
      throw new Exception("Out of the domain")

    valueAt(x)
  }

  protected def valueAt(x: Double): Double
}

class LinearInterpolator(x_coords: Vector[Double],
                         y_coords: Vector[Double])
    extends HandyUnivariateInterpolator(x_coords, y_coords) {

  override protected def valueAt(x: Double): Double = {
    // TODO use binary search

    X.zipWithIndex.find{case (e, i) => e >= x} match {
      case None => throw new Exception("Out of the domain")
      case Some((_, index)) =>
        val w = (x - X(index-1)) / (X(index) - X(index-1))
        Y(index-1)*(1-w) + Y(index)*w
    }
  }
}

*/

//trait UnivariateInterpolator[E] {
//    def apply(x: E): E
//    def apply(x: Vector[E]): Vector[E] = x map apply
//}

abstract class HandyUnivariateInterpolator[T <% Ordered[T]]
    (x_coords: Vector[T],
     y_coords: Vector[T])
    (implicit val field: Field[T],
     implicit val classtag: ClassTag[T]) {//extends UnivariateInterpolator{

  if (x_coords.size != x_coords.toArray.toSet.size)
    throw new Exception("x coordinates must be unique")
  if (x_coords.size != y_coords.size)
    throw new Exception("x_coords and y_coords must be of the same size")
  if (x_coords.size == 0)
    throw new Exception("need to provide at least one pair of coordinates")

  private val nodes = x_coords.toArray zip y_coords.toArray sortBy {n => n._1}
  protected val X: Array[T] = nodes map {n => n._1}
  protected val Y: Array[T] = nodes map {n => n._2}

  def apply(x: T): T = {
    if (x < X(0) || x > X(X.size - 1))
      throw new Exception("Out of the domain")

    valueAt(x)
  }

  protected def valueAt(x: T): T

  def apply(x: Vector[T])(implicit canMapValues: CanMapValues[Vector[T], T, T, Vector[T]]): Vector[T] = x map apply

}


class LinearInterpolator[T <% Ordered[T]]
    (x_coords: Vector[T],
     y_coords: Vector[T])
    (implicit field: Field[T],
     classtag: ClassTag[T])
    extends HandyUnivariateInterpolator[T](x_coords, y_coords) {

  override protected def valueAt(x: T): T = {
    // TODO use binary search

    X.zipWithIndex.find{case (e, i) => e >= x} match {
      case None => throw new Exception("Out of the domain")
      case Some((_, index)) =>
        val w = field./(field.-(x,
                                X(index-1)),
                        field.-(X(index),
                                X(index-1)))
        val u = field.-(field.one, w)
        field.+(field.*(Y(index-1), u),
                field.*(Y(index), w))
    }
  }
}
