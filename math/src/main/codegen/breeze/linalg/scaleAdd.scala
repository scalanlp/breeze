package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import spire.syntax.cfor.cfor

/**
 * method for representing scaleAdd(y, a, x) == y + a * x
 *
 * This method is designed to eventually replace [[breeze.linalg.axpy]]
 *
 * @author dlwh
 * */
object scaleAdd extends UFunc {

  @expand
  implicit def scaleAddArray[@expand.args(Int, Double, Long, Float) T]: InPlaceImpl3[Array[T], T, Array[T]] =
    new InPlaceImpl3[Array[T], T, Array[T]] {
      def apply(v: Array[T], v2: T, v3: Array[T]): Unit = {
        require(v.length == v3.length, "Arrays must have the same length!")
        cfor(0)(_ < v.length, _ + 1) { i =>
          v(i) += v2 * v3(i)
        }
      }
    }

}
