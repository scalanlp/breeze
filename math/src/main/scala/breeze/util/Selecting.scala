package breeze.util

import breeze.macros.expand
import breeze.stats.distributions.Rand

/**Quickselect for linear-time medians, etc.
  * See [[scala.util.Sorting]] and [[breeze.util.Sorting]]
 * @author ktakagaki
 * @date 05/09/2014.
 */
object Select {

  /** Quickselect from an array of T. */
  @expand
  def quickSelect[@expand.args(Int, Long, Double, Float) T](a: Array[T], position: Int): T = {
    quickSelectImpl(a.clone(), position)._1
  }

  /** Quickselect from an array of T. */
  @expand
  def quickSelectWithIndex[@expand.args(Int, Long, Double, Float) T](a: Array[T], position: Int): (T, Int) = {
    quickSelectImpl(a.clone(), position)
  }

  @expand
  def quickSelectImpl[@expand.args(Int, Long, Double, Float) T](x: Array[T], position: Int): (T, Int) = {

    var pivotIndex = -1
    implQuickSelectSort(x, position)
    (x(pivotIndex), pivotIndex) // return

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
        if( x(index) > pivotVal ){
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

  }

}
