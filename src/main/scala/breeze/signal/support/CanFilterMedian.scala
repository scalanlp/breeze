package breeze.signal.support

import breeze.signal.{filterMedian, OptOverhang}
import breeze.linalg.{convert, median, DenseVector}
import breeze.macros.expand
import scala.collection.mutable
import breeze.numerics.isOdd

/**A relatively optimized median filter, using TreeSet
 * @author ktakagaki
 * @date 2/28/14.
 */
trait CanFilterMedian[Input] {
  def apply(data: DenseVector[Input], windowLength: Int, overhang: OptOverhang): DenseVector[Input]
}

object CanFilterMedian {

  /** Use via implicit delegate syntax filterBP(x: DenseVector) and filterBS(x: DenseVector)
    *
    */
  //Int, Long and Float will calculate in Double (see algorithm, needs infinitesimal small numbers for ordering)
  @expand
  implicit def dvFilterMedianT[@expand.args(Int, Long, Float) T]: CanFilterMedian[T] = {

    new CanFilterMedian[T] {
      def apply(data: DenseVector[T], windowLength: Int, overhang: OptOverhang): DenseVector[T] = {
        convert( filterMedian( convert(data, Double), windowLength, overhang ), T)
      }
    }

  }

  //Double returns Double
  implicit def dvFilterMedianDouble: CanFilterMedian[Double] = {

    new CanFilterMedian[Double] {
      def apply(data: DenseVector[Double], windowLength: Int, overhang: OptOverhang): DenseVector[Double] = {

        require(isOdd(windowLength), "median filter can only take odd windowLength values, since even values will cause a half-frame time shift")

        val threadNo = 8 //reasonable for modern processors

        val windowLengthPre = windowLength/2
        val splitDataLength = data.length/threadNo

        var tempret =
          if( splitDataLength > windowLength*10*threadNo ){    //arbitrary cutoff for whether to parallelize or not

          //middle 6 data vectors
          var splitData: Vector[Vector[Double]] = (
            for(cnt <- 1 to threadNo - 2)
              yield data.slice(cnt*splitDataLength - windowLengthPre, (cnt+1)*splitDataLength + windowLengthPre).toScalaVector
          ).toVector

          splitData = splitData.+:( //first data vector
            data.slice(0, splitDataLength + windowLengthPre).toScalaVector
          )
          splitData = splitData.:+(
            data.slice((threadNo-1)*splitDataLength - windowLengthPre, data.length).toScalaVector
          )

          //if( isOdd(windowLength) )
            splitData.par.flatMap( medianFilterImplOddNoOverhang(_, windowLength) ).toVector
          //else splitData.par.flatMap( medianFilterImplEvenDoubleNoOverhang(_, windowLength) )

        } else {

          //if( isOdd(windowLength) )
            medianFilterImplOddNoOverhang(data.toScalaVector, windowLength).toVector
          //else medianFilterImplEvenDoubleNoOverhang(data.toScalaVector, windowLength)

        }

      tempret = overhang match {
        case OptOverhang.PreserveLength => {
          val halfWindow = (windowLength - 1)/2//(windowLength+1)/2 - 1

          //pad both sides of the vector with medians with smaller windows
          (for(winLen <- 0 to halfWindow-1) yield median( data(0 to winLen * 2) )).toVector ++ tempret ++
          (for(winLen <- (- halfWindow) to -1 ) yield median( data( 2*winLen + 1 to -1) )).toVector
        }
        case OptOverhang.None => tempret
        case opt: OptOverhang => {
          throw new IllegalArgumentException("filterMedian only supports overhang=OptOverhang.PreserveLength/None, does not support " + opt.toString )
        }
      }

        DenseVector( tempret.toArray )

      }


    }
  }


  /**Implementation, odd window*/
  def medianFilterImplOddNoOverhang(data: Vector[Double], windowLength: Int): Vector[Double] = {
    require(windowLength <= data.length)
    require(windowLength % 2 == 1)

    val middleIndex = windowLength/2  //(windowLength+1)/2 - 1

    //The queue stores data values in order, to facilitating popping from the TreeSet once the window passes by
    val queue = new mutable.Queue[Double]()
    //The TreeSet stores values within the current window, sorted, so that the median can be found easily
    //tried various collection classes, but TreeSet implementation is fastest by far
    var sortedData = new mutable.TreeSet[Double]()

    def addData(x: Double) = {
      val adjustedX: Double = adjustX(x) //adjusts the value slightly, so that equal values can be written into the TreeSet
      queue.enqueue( adjustedX )
      sortedData.+=( adjustedX )// = sortedData.+(adjustedX)
    }

    //recursive function to adjust values slightly, since TreeSet will ignore added values if they are already present
    def adjustX(x: Double): Double = if(sortedData.contains(x)) adjustX( x * 1.0000000001 + 1E-295 ) else x
//    def adjustX(x: Float): Float= if(sortedData.contains(x)) adjustX( x * 1.00001f + 1E-30f ) else x

    //add data points from the first window to the TreeSet
    for(cnt <- 0 until windowLength) addData( data(cnt) )

    //initialize return array
    val tempret = new Array[Double]( data.length - (windowLength - 1) )

    //loop variables
    var firstElement = 0
    var lastElementExclusive = windowLength

    while( firstElement < tempret.length - 1 ){
      //set current middle variable
      tempret(firstElement) = sortedData.toStream.apply(middleIndex)//toStream seems to be fastest way to access middle value

      val remove = queue.dequeue()
      sortedData = sortedData.-=(remove)//-(remove)
      addData( data(lastElementExclusive) )

      firstElement += 1
      lastElementExclusive += 1
    }
    //process last element separately
    tempret(firstElement) = sortedData.toStream.apply(middleIndex)

    tempret.toVector

  }

//  /**Implementation, even window*/
//  private def medianFilterImplEvenDoubleNoOverhang(data: Vector[Double], windowLength: Int): Vector[Double] = {
//    require(windowLength <= data.length)
//    require(windowLength % 2 == 0)
//
//    val middleFirstIndex = windowLength/2 - 1
//
//    //The queue stores data values in order, to facilitating popping from the TreeSet once the window passes by
//    val queue = new mutable.Queue[Double]()
//    //The TreeSet stores values within the current window, sorted, so that the median can be found easily
//    //tried various collection classes, but TreeSet implementation is fastest by far
//    var sortedData = new mutable.TreeSet[Double]()
//
//    def addData(x: Double) = {
//      val adjustedX = adjustX(x) //adjusts the value slightly, so that equal values can be written into the TreeSet
//      queue.enqueue( adjustedX )
//      sortedData.+=( adjustedX )// = sortedData.+(adjustedX)
//    }
//
//    //recursive function to adjust values slightly, since TreeSet will ignore added values if they are already present
//    def adjustX(x: Double): Double = if(sortedData.contains(x)) adjustX( x * 1.00000000001 + 1E-300 ) else x
//
//    //add data points from the first window to the TreeSet
//    for(cnt <- 0 until windowLength) addData( data(cnt) )
//
//    //initialize return array
//    val tempret = new Array[Double]( data.length - (windowLength - 1) )
//
//    //loop variables
//    var firstElement = 0
//    var lastElementExclusive = windowLength
//
//    while( firstElement < tempret.length ){
//      //set current middle variable
//      val tempStream = sortedData.toStream
//      tempret(firstElement) = (tempStream.apply(middleFirstIndex) + tempStream.apply(middleFirstIndex + 1))/2d
//
//      val remove = queue.dequeue()
//      sortedData = sortedData.-=(remove)//-(remove)
//      addData( data(lastElementExclusive) )
//
//      firstElement += 1
//      lastElementExclusive += 1
//    }
//
//    Vector(tempret)
//
//  }


}



