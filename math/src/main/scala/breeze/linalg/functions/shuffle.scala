package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.{DenseMatrix, randomInt, DenseVector}
import breeze.macros.expand
import spire.math.Complex
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import breeze.stats.distributions.Rand

/**Returns the given DenseVector, Array, or DenseMatrix as a shuffled copy. (Fisher-Yates shuffle)
 * @author ktakagaki
 * @date 05/12/2014.
 */
object shuffle extends UFunc {

  @expand
  @expand.valify
  implicit def implShuffle_Arr_eq_Arr[@expand.args(Int, Double, Long, Float, Complex) T]: Impl[Array[T], Array[T]] =
  new Impl[Array[T], Array[T]] {

    protected val _classTag: ClassTag[T] = scala.reflect.classTag[T]

    def apply(arr: Array[T]): Array[T] = {
      //copy
      val tempret = arr.clone()
      val rand = Rand.randInt(tempret.length)

      var count = 0
      while(count < tempret.length) {
        swap(tempret, count, rand.get())
        count += 1
      }
      tempret
    }

    def swap(arr: Array[T], indexA: Int, indexB: Int): Unit = {
      val temp = arr(indexA)
      arr(indexA) = arr(indexB)
      arr(indexB) = temp
    }

  }

  @expand
  @expand.valify
  implicit def implShuffle_DV_eq_DV[@expand.args(Int, Double, Long, Float, Complex) T]: Impl[DenseVector[T], DenseVector[T]] =
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(dv: DenseVector[T]): DenseVector[T] = DenseVector( shuffle(dv.toArray) )
    }


  @expand
  @expand.valify
  implicit def implShuffle_DM_eq_DM[@expand.args(Int, Double, Long, Float, Complex) T]: Impl[DenseMatrix[T], DenseMatrix[T]] =
    new Impl[DenseMatrix[T], DenseMatrix[T]] {

      def apply(dm: DenseMatrix[T]): DenseMatrix[T] = {
        val rows = dm.rows
        val cols = dm.cols
        new DenseMatrix(rows, cols, shuffle(dm.toArray))
      }
    }


}
