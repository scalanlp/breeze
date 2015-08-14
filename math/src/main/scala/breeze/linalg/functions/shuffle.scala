package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import spire.math.Complex
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import breeze.stats.distributions.Rand

/**Returns the given DenseVector, Array, or DenseMatrix as a shuffled copy. (Fisher-Yates shuffle)
  * @author ktakagaki
  * @date 05/12/2014.
  */
object shuffle extends UFunc {

  implicit def implShuffle_Arr_eq_Arr[T](implicit ct: ClassTag[T]): Impl[Array[T], Array[T]] = {
    new Impl[Array[T], Array[T]] {

      def apply(arr: Array[T]): Array[T] = {
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
  }



  implicit def implShuffle_Coll_eq_Coll[Coll, T, CollRes](implicit view: Coll <:< IndexedSeq[T],
                                                          cbf: CanBuildFrom[Coll, T, CollRes]) : Impl[Coll, CollRes] = {
    new Impl[Coll, CollRes] {

      override def apply(v: Coll): CollRes = {
        val builder = cbf(v)
        val copy = v.to[ArrayBuffer]
        val rand = Rand.randInt(copy.length)

        var count = 0
        while (count < copy.length) {
          swap(copy, count, rand.get())
          count += 1
        }

        builder ++= copy
        builder.result()
      }

      def swap(arr: ArrayBuffer[T], indexA: Int, indexB: Int): Unit = {
        val temp = arr(indexA)
        arr(indexA) = arr(indexB)
        arr(indexB) = temp
      }

    }
  }

  implicit def implShuffle_DV_eq_DV[T](implicit arrImpl: Impl[Array[T], Array[T]],
                                       ct: ClassTag[T]): Impl[DenseVector[T], DenseVector[T]] = {
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(dv: DenseVector[T]): DenseVector[T] = DenseVector(shuffle(dv.toArray))
    }
  }


  implicit def implShuffle_DM_eq_DM[T](implicit arrImpl: Impl[Array[T], Array[T]],
                                       ct: ClassTag[T]): Impl[DenseMatrix[T], DenseMatrix[T]] = {
    new Impl[DenseMatrix[T], DenseMatrix[T]] {

      def apply(dm: DenseMatrix[T]): DenseMatrix[T] = {
        val rows = dm.rows
        val cols = dm.cols
        new DenseMatrix(rows, cols, shuffle(dm.toArray))
      }
    }
  }


}
