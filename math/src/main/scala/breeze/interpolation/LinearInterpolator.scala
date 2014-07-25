package breeze.interpolation

/**
 *
 * @author chrismedrela
 */

import scala.reflect.ClassTag

import breeze.linalg._
import breeze.math.Field

class LinearInterpolator[T:ClassTag:Field:Ordering]
    (x_coords: Vector[T],
     y_coords: Vector[T])
    extends HandyUnivariateInterpolator[T](x_coords, y_coords) {

  private val ord = implicitly[Ordering[T]]
  import ord.mkOrderingOps

  override protected def interpolate(x: T): T = {
    val index = bisearch(x)

    if (index == 0) Y(0)
    else interpolate(index, x)
  }

  override protected def extrapolate(x: T): T = {
    if (X.length < 2) {
      throw new IndexOutOfBoundsException("Cannot extrapolate linearly when given less than two points.")
    }

    val index = if (x < X(0)) 1 else X.length-1
    interpolate(index, x)
  }

  private def interpolate(index: Int, x: T): T = {
    /* Interpolate or extrapolate linearly between point number index-1 and index. */
    assert(index > 0)

    val x1 = X(index-1)
    val x2 = X(index)
    val y1 = Y(index-1)
    val y2 = Y(index)
    val f = implicitly[Field[T]]

    // w = (x - x1) / (x2 - x1)
    val w = f./(f.-(x, x1),
                f.-(x2, x1))
    // u = 1 - w
    val u = f.-(f.one, w)

    // result = y1 * u + y2 * w
    f.+(f.*(y1, u),
        f.*(y2, w))
  }
}

object LinearInterpolator {
  def apply[T:ClassTag:Field:Ordering](
     x_coords: Vector[T],
     y_coords: Vector[T]) = new LinearInterpolator(x_coords, y_coords)
}
