package breeze.linalg

import breeze.generic.UFunc

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import breeze.stats.distributions.{Rand, RandBasis}

/**
  * Return the given DenseVector, Array, or DenseMatrix as a shuffled copy
  * by using Fisher-Yates shuffle.
  * Additionally, can return the given Array as a shuffled copy with the
  * corresponding shuffle index information, or return the given Array as a
  * shuffled copy using the inverse of the given shuffle index information,
  * reversing the shuffle.
  *
  * @author ktakagaki
  * @date 05/12/2014
  * @author huaminli
  * @date 08/01/2016
  */
object shuffle extends UFunc {

  implicit def implShuffle_Arr_eq_Arr[T](implicit ct: ClassTag[T], rb: RandBasis = Rand):
  Impl[Array[T], Array[T]] = {
    new Impl[Array[T], Array[T]] {
      /**
        * Shuffle the given [[Array[T]]].
        * @param arr the given array stored as [[Array[T]]]
        * @return a shuffled array stored as [[Array[T]]]
        */
      def apply(arr: Array[T]): Array[T] = {
        // Make a copy of the input.
        val tempret = arr.clone()

        // Shuffle tempret via Fisher-Yates method.
        var count = tempret.length - 1
        while(count > 0) {
          swap(tempret, count, rb.randInt(count + 1).get())
          count -= 1
        }
        tempret
      }

      /**
        * Swap two elements of [[Array[T]]] with specified indices [[Int]].
        * @param arr the given array stored as [[Array[T]]]
        * @param indexA the first given [[Int]] index
        * @param indexB the second given [[Int]] index
        */
      def swap[T](arr: Array[T], indexA: Int, indexB: Int): Unit = {
        val temp = arr(indexA)
        arr(indexA) = arr(indexB)
        arr(indexB) = temp
      }
    }
  }

  implicit def implShuffle_Arr_Arr_Boolean_eq_Arr[T](implicit ct: ClassTag[T]):
  Impl3[Array[T], Array[Int], Boolean, Array[T]] = {
    new Impl3[Array[T], Array[Int], Boolean, Array[T]] {
      /**
        * shuffle the given [[Array[T]]] arr according to the given
        * permutation [[Array[Int]]] arrIndex if [[Boolean]] isInverse is
        * false; shuffle the given [[Array[T]]] arr according to the inverse of
        * the given permutation [[Array[Int]]] arrIndex if [[Boolean]]
        * isInverse is true.
        * @param arr the given array stored as [[Array[T]]]
        * @param arrIndex the given permutation array stored as [[Array[Int]]]
        * @param isInverse the indicator whether perform inverse shuffle
        * @return a shuffled array stored as [[Array[T]]]
        */
      def apply(arr: Array[T], arrIndex: Array[Int],
                isInverse: Boolean): Array[T] = {
        require(arr.length == arrIndex.length,
          "The two input arrays should have the same length!")
        // Make a copy of the input.
        val tempret = new Array[T](arr.length)

        if (!isInverse) {
          // Shuffle tempret via given permutation.
          for (i <- arr.indices) {
            tempret(i) = arr(arrIndex(i))
          }
        } else {
          // Inverse shuffle tempret via given permutation.
          for (i <- arr.indices) {
            tempret(arrIndex(i)) = arr(i)
          }
        }
        tempret
      }
    }
  }

  implicit def implShuffle_Arr_Arr_eq_Arr[T](implicit ct: ClassTag[T]):
  Impl2[Array[T], Array[Int], Array[T]] = {
    new Impl2[Array[T], Array[Int], Array[T]] {
      /**
        * Shuffle the given [[Array[T]] arr according to the given
        * permutation [[Array[Int]]] arrIndex.
        * @param arr the given array stored as [[Array[T]]]
        * @param arrIndex the given permutation array stored as [[Array[Int]]]
        * @return a shuffled array stored as [[Array[T]]]
        */
      def apply(arr: Array[T], arrIndex: Array[Int]): Array[T] = {
        // Shuffle the input via given permutation.
        shuffle(arr, arrIndex, false)
      }
    }
  }

  implicit def implShuffle_Coll_eq_Coll[Coll, T, CollRes](implicit view:
  Coll <:< IndexedSeq[T], cbf: CanBuildFrom[Coll, T, CollRes], rb: RandBasis = Rand) :
  Impl[Coll, CollRes] = {
    new Impl[Coll, CollRes] {
      /**
        * Shuffle the given [[Coll]].
        * @param v the given collection stored as [[Coll]]
        * @return a shuffled collection stored as [[CollRes]]
        */
      override def apply(v: Coll): CollRes = {
        // Make a copy of the input.
        val builder = cbf(v)
        val copy = v.to[ArrayBuffer]

        // Shuffle tempret via Fisher-Yates method.
        var count = copy.length - 1
        while (count > 0) {
          swap(copy, count, rb.randInt(count + 1).get())
          count -= 1
        }
        builder ++= copy
        builder.result()
      }

      /**
        * Swap two elements of [[Coll]] with specified indices [[Int]].
        * @param arr the given array stored as [[Array[T]]]
        * @param indexA the first given [[Int]] index
        * @param indexB the second given [[Int]] index
        */
      def swap(arr: ArrayBuffer[T], indexA: Int, indexB: Int): Unit = {
        val temp = arr(indexA)
        arr(indexA) = arr(indexB)
        arr(indexB) = temp
      }

    }
  }

  implicit def implShuffle_DV_eq_DV[T](implicit arrImpl:
  Impl[Array[T], Array[T]], ct: ClassTag[T], rb: RandBasis = Rand):
  Impl[DenseVector[T], DenseVector[T]] = {
    new Impl[DenseVector[T], DenseVector[T]] {
      /**
        * Shuffle the given [[DenseVector[T]]].
        * @param dv the given vector stored as [[DenseVector[T]]]
        * @return a shuffled vector stored as [[DenseVector[T]]]
        */
      def apply(dv: DenseVector[T]): DenseVector[T] =
        // convert to array and perform the shuffling.
        new DenseVector(shuffle(dv.toArray))
    }
  }

  implicit def implShuffle_DM_eq_DM[T](implicit arrImpl:
  Impl[Array[T], Array[T]], ct: ClassTag[T], rb: RandBasis = Rand):
  Impl[DenseMatrix[T], DenseMatrix[T]] = {
    new Impl[DenseMatrix[T], DenseMatrix[T]] {
      /**
        * Shuffle the given [[DenseMatrix[T]]].
        * @param dm the given matrix stored as [[DenseMatrix[T]]]
        * @return a shuffled matrix stored as [[DenseMatrix[T]]]
        */
      def apply(dm: DenseMatrix[T]): DenseMatrix[T] = {
        // convert to array and perform the shuffling.
        val rows = dm.rows
        val cols = dm.cols
        new DenseMatrix(rows, cols, shuffle(dm.toArray))
      }
    }
  }
}
