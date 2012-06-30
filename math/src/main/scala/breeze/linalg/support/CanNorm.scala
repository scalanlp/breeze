package breeze.linalg
package support

import breeze.math.Ring


/**
 * Construction delegate for getting the norm of a value of type From.
 *
 * @author dramage
 */
trait CanNorm[-From] {
  def apply(v1: From, v2: Double): Double
}

object CanNorm {
  implicit def mkTensor1Norm[T, V](implicit tt : T=>Vector[V], ring: Ring[V]): CanNorm[T] = new CanNorm[T] {
    def apply(t : T, n : Double) : Double =
      tt(t).norm(n)
  }
}
