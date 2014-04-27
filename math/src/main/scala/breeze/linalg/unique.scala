package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import spire.implicits._

/**
 * roll the array
 *
 * @author stucchio
 */
object unique extends UFunc {

  @expand
  implicit def impl[@expand.args(Int, Double, Float, Long) S]: Impl[DenseVector[S], DenseVector[S]] = new Impl[DenseVector[S], DenseVector[S]] {
    def apply(v: DenseVector[S]): DenseVector[S] = if (v.size > 0) {
      val data = new Array[S](v.size)
      cfor(0)(i => i < v.size, i => i+1)(i => {
        data(i) = v.unsafeValueAt(i)
      })
      java.util.Arrays.sort(data)

      var elementCount = 1
      var lastElement = data(0)
      cfor(0)(i => i < data.size, i => i+1)(i => {
        val di = data(i)
        if (di != lastElement) {
          elementCount += 1
          lastElement = di
        }
      })

      val result = new Array[S](elementCount)
      result(0) = data(0)
      lastElement = data(0)
      var idx = 1
      cfor(0)(i => i < data.size, i => i+1)(i => {
        val di = data(i)
        if (di != lastElement) {
          result(idx) = di
          lastElement = di
          idx += 1
        }
      })

      DenseVector(result)
    } else {
      DenseVector(new Array[S](0))
    }
  }

}
