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
    def bisearch(low: Int, high: Int): Int = (low+high)/2 match {
      case mid if low == high => mid
      case mid if X(mid) < x => bisearch(mid+1, high)
      case mid => bisearch(low, mid)
    }
    val index = bisearch(0, X.length-1)

    if (index == 0) {
      Y(0)
    } else {
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

  override protected def extrapolate(x: T): T = {
    if (X.length < 2) {
      throw new IndexOutOfBoundsException("Cannot extrapolate linearly when given less than two points.")
    }

    val index = if (x < X(0)) 1 else X.length-1

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

  private def interpolate(x1, x2, y1, y2, x) = {
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
