package breeze.interpolation

/**
 *
 * @author chrismedrela
 */
import scala.reflect.ClassTag

import breeze.linalg._
import breeze.math.Field
import breeze.storage.Zero

class CubicInterpolator(x_coords: Vector[Double], y_coords: Vector[Double])
    extends HandyUnivariateInterpolator[Double](x_coords, y_coords) {

  if (X.length < 3)
    throw new Exception("You must provide at least 3 points for CubicInterpolator.")

  private def h(k: Int): Double = X(k + 1) - X(k)
  private def d(k: Int): Double = (Y(k + 1) - Y(k)) / h(k)
  private def lambda(k: Int): Double = h(k) / (h(k - 1) + h(k))
  private def ro(k: Int): Double = 1 - lambda(k)

  private val M: DenseMatrix[Double] = DenseMatrix.tabulate(X.length - 2, X.length - 2) {
    case (i, j) if j - i == -1 => ro(i + 1) // one cell to left from the diagonal
    case (i, j) if j == i => 2 // on the diagonal
    case (i, j) if j - i == 1 => lambda(i + 1) // one cell to right from the diagonal
    case _ => 0
  }
  private val b = DenseVector.tabulate(X.length - 2) {
    case i => 6 * (d(i + 1) - d(i)) / (h(i) + h(i + 1))
  }
  private val mp = M \ b
  private def m(i: Int) = i match {
    case 0 => 0
    case i if i == X.length - 1 => 0
    case i => mp(i - 1)
  }
  private val A = DenseMatrix.tabulate(X.length - 1, 4) {
    case (k, 0) => Y(k)
    case (k, 1) => d(k) - h(k) / 6 * (2 * m(k) + m(k + 1))
    case (k, 2) => m(k) / 2
    case (k, 3) => (m(k + 1) - m(k)) / 6 / h(k)
    case _ => assert(false, "unreachable"); ???
  }

  override protected def interpolate(x: Double): Double = {
    val index = bisearch(x) - 1

    if (index == -1) Y(0)
    else {
      val dx = x - X(index)
      A(index, 0) + A(index, 1) * dx + A(index, 2) * dx * dx + A(index, 3) * dx * dx * dx
    }
  }
}

object CubicInterpolator {
  def apply(x_coords: Vector[Double], y_coords: Vector[Double]) = new CubicInterpolator(x_coords, y_coords)
}
