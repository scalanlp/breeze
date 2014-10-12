package breeze.signal.filter.support

import breeze.linalg.DenseVector
import breeze.macros.expand
import breeze.numerics.isOdd
import breeze.signal.OptOverhang
import breeze.stats._
import breeze.util.quickSelectImpl

/**A relatively optimized median filter, using TreeSet
 * @author ktakagaki
 * @date 2/28/14.
 */
trait CanFilterMedian[Input] {
  def apply(data: DenseVector[Input], windowLength: Int, overhang: OptOverhang): DenseVector[Input]
}

object CanFilterMedian {

  //Int, Long and Float will calculate in Double (see algorithm, needs infinitesimal small numbers for ordering)
  @expand
  implicit def dvFilterMedianT[@expand.args(Int, Long, Double, Float) T]: CanFilterMedian[T] =

    new CanFilterMedian[T] {
      def apply(data: DenseVector[T], windowLength: Int, overhang: OptOverhang): DenseVector[T] = {

        require(isOdd(windowLength), "median filter can only take odd windowLength values, since even values will cause a half-frame time shift")
        require(data.length >= 3, "data must be longer than 3")
        require(windowLength >= 1, "window length must be longer than 1")

        if( windowLength == 1 ) data.copy
        else {

          var tempret = new Array[T](data.length)
          val halfWindow = (windowLength-1)/2
          var index = halfWindow

          overhang match{
            case OptOverhang.PreserveLength => {
              //calculate beginning and end separately, for partial-windows (no overhang)
              for( indexFromBeginning <- 0 until halfWindow ) tempret(indexFromBeginning) = median( data(0 to indexFromBeginning*2) )
              for( indexToEnd <- 0 until halfWindow ) tempret(data.length-indexToEnd-1) = median( data(data.length-2*indexToEnd-1 until data.length) )
            }
            case OptOverhang.None => {}
            case opt: OptOverhang => throw new IllegalArgumentException("Option " + opt + " is invalid here.")
          }

          //first full-window value must be initialized separately
          index = halfWindow
          val tempDataExtract = data(0 to index + halfWindow).toArray
          var currentMedian = quickSelectImpl(tempDataExtract, halfWindow)
          tempret(index) = currentMedian
          index += 1

          while( index < data.length - halfWindow ){
            //data value which the window has passed by
            val nowObsoleteWindowValue: T = data(index-halfWindow-1)
            val newWindowValue: T = data(index+halfWindow)

            //if the obsolete value is not equal to the new value...
            if( nowObsoleteWindowValue != newWindowValue ) {
              //replace now obsolete value with new data value within temporary array
              findAndReplaceInstanceInPlace(tempDataExtract, nowObsoleteWindowValue, newWindowValue, halfWindow)
              //if the new value and old value lie on different sides of the current Median,
              if( (nowObsoleteWindowValue >= currentMedian || newWindowValue >= currentMedian)
                    && (nowObsoleteWindowValue <= currentMedian || newWindowValue <= currentMedian) ){
                //then the median needs to be recalculated
                currentMedian = quickSelectImpl(tempDataExtract, halfWindow)
              }
            }
            //...if the two values are the same, do nothing

            tempret(index) = currentMedian
            index += 1
          }

          overhang match {
            case OptOverhang.PreserveLength => DenseVector(tempret)
            case OptOverhang.None => DenseVector(tempret.slice(halfWindow, data.length - halfWindow))
          }
        }

      }

      def findAndReplaceInstanceInPlace( arr: Array[T], fromValue: T, toValue: T, pivotPoint: Int): Unit = {
        val pivotValue: T = arr(pivotPoint)
        var found = false

        if( fromValue == pivotValue ) {
          arr(pivotPoint) = toValue
          found = true
        } else if( fromValue < pivotValue ){
          var count = pivotPoint - 1
          while( count >= 0 ){
            if( arr(count) == fromValue ) {
              arr(count) = toValue
              count = Int.MinValue
              found = true
            }else {
              count -= 1
            }
          }
        } else { //if( fromValue > pivotValue ){
          var count = pivotPoint + 1
          while( count < arr.length ){
            if( arr(count) == fromValue ){
              arr(count) = toValue
              count = Int.MaxValue
              found = true
            }else {
              count += 1
            }
          }
        }

        require(found, "The fromValue was not found within the given array, something is wrong!")
      }

    }



}
