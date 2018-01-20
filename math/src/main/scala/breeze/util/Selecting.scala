package breeze.util

import breeze.macros.expand
import breeze.generic.UFunc
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**Quickselect for linear-time medians, etc.
 * See [[scala.util.Sorting]] and [[breeze.util.Sorting]]
 * @author ktakagaki, dlwh
 */
object quickSelect extends UFunc {

//  /** Quickselect from an array of T. */

  implicit def implFromQSInPlace[T](implicit op: quickSelect.InPlaceImpl2[Array[T], Int]): Impl2[Array[T], Int, T] = {
    new Impl2[Array[T], Int, T] {
      def apply(a: Array[T], position: Int): T = {
        val quickselected: Array[T] = a.clone()
        op(quickselected, position)
        quickselected(position)
      }
    }
  }

  @expand
  implicit def inPlaceImpl2[@expand.args(Int, Long, Double, Float) T]: InPlaceImpl2[Array[T], Int] = {

    new InPlaceImpl2[Array[T], Int] {

      def apply(x: Array[T], position: Int): Unit = {

        var pivotIndex = -1

        def implQuickSelectSort(x: Array[T], position: Int): Unit = {
          var left = 0
          var right = x.length - 1
          require(
            position >= left && position <= right,
            "Invalid position specification: " + position + " with array length: " + x.length)

          while (pivotIndex != position && right >= left) {
            val pvt = med3(left, right, ((left.toLong + right) / 2).toInt)
            pivotIndex = partition(x, left, right, pvt)
            if (pivotIndex < position) left = pivotIndex + 1
            else if (pivotIndex > position) right = pivotIndex - 1
          }
        }

        def med3(p1: Int, p2: Int, p3: Int) = {
          if (x(p1) < x(p2)) {
            if (x(p2) < x(p3)) p2 else if (x(p1) < x(p3)) p3 else p1
          } else {
            if (x(p2) > x(p3)) p2 else if (x(p1) > x(p3)) p3 else p1
          }
        }

        def partition(x: Array[T], left: Int, right: Int, pivot: Int): Int = {
          val pivotVal = x(pivot)
          swap(pivot, right)
          var storeIndex = left

          var index = left
          while (index < right) {
            if (x(index) < pivotVal) {
              swap(index, storeIndex)
              storeIndex += 1
            }
            index += 1
          }
          swap(right, storeIndex)

          storeIndex
        }

        def swap(a: Int, b: Int) {
          val t = x(a)
          x(a) = x(b)
          x(b) = t
        }

        implQuickSelectSort(x, position)
      }
    }

  }

  implicit def implFromQSInPlaceColl[Coll, T](
      implicit view: Coll <:< Seq[T],
      ordering: Ordering[T]): Impl2[Coll, Int, T] = {
    new Impl2[Coll, Int, T] {
      def apply(a: Coll, position: Int): T = {
        val copy = view(a).to[ArrayBuffer]
        inPlace(copy, position)
        copy(position)
      }
    }
  }

  implicit def implFromOrdering[T, Coll](
      implicit view: Coll <:< mutable.IndexedSeq[T],
      ordering: Ordering[T]): InPlaceImpl2[Coll, Int] = {
    new InPlaceImpl2[Coll, Int] {
      import ordering.mkOrderingOps

      def apply(rawx: Coll, position: Int): Unit = {

        val coll = view(rawx)
        var pivotIndex = -1

        def implQuickSelectSort(x: mutable.IndexedSeq[T], position: Int): Unit = {
          var left = 0
          var right = x.length - 1
          require(
            position >= left && position <= right,
            "Invalid position specification: " + position + " with coll length: " + x.length)

          while (pivotIndex != position && right >= left) {
            val pvt = med3(left, right, ((left.toLong + right) / 2).toInt)
            pivotIndex = partition(x, left, right, pvt)
            if (pivotIndex < position) left = pivotIndex + 1
            else if (pivotIndex > position) right = pivotIndex - 1
          }

          def med3(p1: Int, p2: Int, p3: Int) = {
            if (x(p1) < x(p2)) {
              if (x(p2) < x(p3)) p2 else if (x(p1) < x(p3)) p3 else p1
            } else {
              if (x(p2) > x(p3)) p2 else if (x(p1) > x(p3)) p3 else p1
            }
          }
        }

        def partition(x: mutable.IndexedSeq[T], left: Int, right: Int, pivot: Int): Int = {
          val pivotVal = x(pivot)
          swap(pivot, right)
          var storeIndex = left

          var index = left
          while (index < right) {
            if (ordering.lt(x(index), pivotVal)) {
              swap(index, storeIndex)
              storeIndex += 1
            }
            index += 1
          }
          swap(right, storeIndex)

          storeIndex
        }

        def swap(a: Int, b: Int) {
          val t = coll(a)
          coll(a) = coll(b)
          coll(b) = t
        }

        implQuickSelectSort(coll, position)
      }
    }

  }

}

/**quickSelectImpl does not clone the input array before doing a quickSelect-sort but instead
 * swaps in place, and therefore,
 * allows other functions to access the intermediate results of the sorting procedure.
 *
 * After quickSelectImpl is run, it is guaranteed that the input array will be swapped
 * around such that every number left of position will be equal or smaller than the element at position,
 * and every number right of position will be equal or larger than the element at position.
 *
 * This can be useful when further using the intermediate results downstream.
 * For example, appending an element or updating an element to an array which has already
 * been through `quickSelectImpl` and then re-calculating `quickSelectImpl`
 * will be faster than applying quickSelectImpl de-novo to the original unsorted array.
 *
 */
@deprecated("use quickSelect.inPlace instead", "0.12")
object quickSelectImpl extends UFunc {

  @expand
  implicit def impl[@expand.args(Int, Long, Double, Float) T]: Impl2[Array[T], Int, T] =
    new Impl2[Array[T], Int, T] {

      def apply(x: Array[T], position: Int): T = {

        var pivotIndex = -1

        def implQuickSelectSort(x: Array[T], position: Int): Unit = {
          var left = 0
          var right = x.length - 1
          require(
            position >= left && position <= right,
            "Invalid position specification: " + position + " with array length: " + x.length)

          while (pivotIndex != position && right >= left) {
            val pvt = med3(left, right, ((left.toLong + right) / 2).toInt)
            pivotIndex = partition(x, left, right, pvt)
            if (pivotIndex < position) left = pivotIndex + 1
            else if (pivotIndex > position) right = pivotIndex - 1
          }
        }

        def med3(p1: Int, p2: Int, p3: Int) = {
          if (x(p1) < x(p2)) {
            if (x(p2) < x(p3)) p2 else if (x(p1) < x(p3)) p3 else p1
          } else {
            if (x(p2) > x(p3)) p2 else if (x(p1) > x(p3)) p3 else p1
          }
        }

        def partition(x: Array[T], left: Int, right: Int, pivot: Int): Int = {
          val pivotVal = x(pivot)
          swap(pivot, right)
          var storeIndex = left

          var index = left
          while (index < right) {
            if (x(index) < pivotVal) {
              swap(index, storeIndex)
              storeIndex += 1
            }
            index += 1
          }
          swap(right, storeIndex)

          storeIndex
        }

        def swap(a: Int, b: Int) {
          val t = x(a)
          x(a) = x(b)
          x(b) = t
        }

        implQuickSelectSort(x, position)
        x(pivotIndex) // return

      }
    }

}
