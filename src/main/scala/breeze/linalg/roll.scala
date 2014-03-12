package breeze.linalg

import breeze.generic.UFunc
import scala.reflect.ClassTag
import spire.implicits._

/**
 * roll the array
 *
 * @author stucchio
 */
object roll extends UFunc {

  implicit def impl[T: ClassTag]: Impl2[DenseVector[T], Int, DenseVector[T]] = new Impl2[DenseVector[T], Int, DenseVector[T]] {
    def apply(v: DenseVector[T], n: Int): DenseVector[T] = {
      require(n >= 0)
      require(n < v.size)
      val result = DenseVector(new Array[T](v.size))
      val endOfBeginning = v.size - n
      cfor(0)(j => j < n, j => j + 1)(j => { //Roll end of array to beginning
        val r = v.unsafeValueAt(endOfBeginning + j)
        result.unsafeUpdate(j, r)
      })
      cfor(n)(j => j < v.size, j => j+1)(j => { //Roll beginning of array forward
        val r = v.unsafeValueAt(j-n)
        result.unsafeUpdate(j,r)
      })
      result
    }
  }

}
