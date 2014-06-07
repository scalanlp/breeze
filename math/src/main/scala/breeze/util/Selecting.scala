package breeze.util

import breeze.macros.expand
import breeze.stats.distributions.Rand
import breeze.generic.UFunc

/**Quickselect for linear-time medians, etc.
  * See [[scala.util.Sorting]] and [[breeze.util.Sorting]]
 * @author ktakagaki
 */
object quickSelect extends UFunc  {

//  /** Quickselect from an array of T. */

  implicit def implFromQSInPlace[T](implicit op: quickSelectImpl.Impl2[Array[T], Int, T]):Impl2[Array[T], Int, T] = {
    new Impl2[Array[T], Int, T] {
      def apply(a: Array[T], position: Int): T = quickSelectImpl(a.clone(), position)
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
object quickSelectImpl extends UFunc  {

  @expand
  implicit def impl[@expand.args(Int, Long, Double, Float) T]: Impl2[Array[T], Int, T] =

    new Impl2[Array[T], Int, T] {

      def apply(x: Array[T], position: Int): T = {

        var pivotIndex = -1

        def implQuickSelectSort(x: Array[T], position: Int): Unit = {
          var left = 0
          var right = x.length - 1
          require(position >= left && position <= right, "Invalid position specification: " + position + " with array length: " + x.length)

          while (pivotIndex != position && right >= left) {
            val rand = Rand.randInt(right - left + 1)
            pivotIndex = partition(x, left, right, rand.get() + left)
            if (pivotIndex < position) left = pivotIndex + 1
            else if (pivotIndex > position) right = pivotIndex - 1
          }
        }

        def partition(x: Array[T], left: Int, right: Int, pivot: Int): Int = {
          val pivotVal = x(pivot)
          swap(pivot, right)
          var storeIndex = left

          var index = left
          while( index < right ){
            if( x(index) < pivotVal ){
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
