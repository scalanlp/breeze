package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.util.{ArrayUtil, ReflectionUtil}
import breeze.macros._

/**
 * deduplicates the array
 *
 * @author stucchio
 */
object unique extends UFunc {

  implicit def impl[S]: Impl[DenseVector[S], DenseVector[S]] = new Impl[DenseVector[S], DenseVector[S]] {
    def apply(v: DenseVector[S]): DenseVector[S] = {
      implicit val ct = ReflectionUtil.elemClassTagFromArray(v.data)
      if (v.size == 0) {
        DenseVector(new Array[S](0))
      } else {
        val data = v.toArray
        ArrayUtil.sort(data)

        var elementCount = 1
        var lastElement = data(0)

        cforRange(0 until data.length) { i =>
          val di = data(i)
          if (di != lastElement) {
            elementCount += 1
            lastElement = di
          }
        }

        val result = new Array[S](elementCount)
        result(0) = data(0)
        lastElement = data(0)
        var idx = 1
        cforRange(0 until data.length) { i =>
          val di = data(i)
          if (di != lastElement) {
            result(idx) = di
            lastElement = di
            idx += 1
          }
        }

        DenseVector(result)
      }
    }
  }

}
