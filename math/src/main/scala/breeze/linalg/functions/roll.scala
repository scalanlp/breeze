package breeze.linalg

import breeze.generic.UFunc
import scala.reflect.ClassTag
import breeze.macros._

/**
 * roll the array
 *
 * @author stucchio
 */
object roll extends UFunc {

  implicit def impl[T: ClassTag]: Impl2[DenseVector[T], Int, DenseVector[T]] =
    new Impl2[DenseVector[T], Int, DenseVector[T]] {
      def apply(v: DenseVector[T], n: Int): DenseVector[T] = {
        require(n >= 0)
        require(n < v.size)
        val result = DenseVector(new Array[T](v.size))
        val endOfBeginning = v.size - n
        cforRange(0 until n){j =>
          result(j) = v(endOfBeginning + j)
        }
        cforRange(n until v.size) { j =>
          result(j) = v(j - n)
        }
        result
      }
    }

}
