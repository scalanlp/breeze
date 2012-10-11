package breeze.linalg.benchmark

import com.google.caliper.{Runner, SimpleBenchmark}
import breeze.linalg.DenseVector


/**
 * 
 * @author dlwh
 */
class ConvolutionBenchmark extends SimpleBenchmark {
  /*
  def mytest() {
    val x: AverageFilter = new AverageFilter(3)
    (x.fSize)
    (x.halfFilter)
    //(arr.length)
    val arr = List[Double](1, 2, 3, 1, 2, 3, 1, 2, 3)
    for (al: Double <- arr) print(al + " ")

    val brr = x.filter(arr.toArray)
    (brr.length)
    for (bl: Double <- brr) print(bl + " ")
    var totalt = 0.
    val times = 500
    for (i <- 0 to times) {
      val arrb: Array[Double] = Array.fill(500000)(math.random)
      val t0 = System.nanoTime()
      x.filter(arrb)
      val t1 = System.nanoTime()
      totalt += t1 - t0
    }

    val s = totalt / 1000000000.
    ("AVG FILTER Elapsed time: " + s + "s")
    ("AVG Filter hz : " + times / s + "Hz")
  }
  */

  val filter = Array[Double](-1, -1, -1, -1, -1, 0, 1, 1, 1, 1, 1)
  def timeDefault(reps: Int) = {
    val x2 = new DefaultFilter(filter)

    var i = 0
    var arr: Array[_] = null
    while(i < reps) {
      val arrb: Array[Double] = Array.fill(500000)(math.random)
      arr = x2.filter(arrb)
      i += 1
    }

    arr

  }

  val filter2 = DenseVector[Double](-1, -1, -1, -1, -1, 0, 1, 1, 1, 1, 1)

  // breeze test
  def timeBreeze(reps: Int) = {

    val x2 = new BreezeFilter(filter2)
    var arr: DenseVector[Double] = null

    var i = 0
    while(i < reps) {
      val arrb: DenseVector[Double] = DenseVector.rand(500000)
      arr = x2.filter(arrb)
      i += 1
    }

    arr
  }


  class DefaultFilter(filter: Array[Double]) {

    //check if Filter is odd
    private def checkFilter() {
      val l: Int = filter.length
      if (l % 2 == 0) {
        throw new IllegalArgumentException("filter length must be odd")
      }
    }
    checkFilter
    val fHalf = (filter.length - 1) / 2

    def dotProduct[T <% Double](as: Array[T], pos: Int, bs: Array[T]) = {
      //def dotProduct(as: Array[Double], pos :Int, bs: Array[Double]): Double = {

      var sum = 0.0
      var i = 0
      while (i < bs.size) {
        sum += bs(i) * as(pos + i)
        i += 1
      }
      sum
    }

    def filter(data: Array[Double]): Array[Double] = {
      val myArray = new Array[Double](data.length)
      val length = data.length - filter.length
      var i = 0
      while(i < length) {
        myArray(i + fHalf) = dotProduct(data, i, filter)
        i += 1
      }
      myArray
    }

  }

  class BreezeFilter(filter : DenseVector[Double]) {
    //check if Filter is odd
    private def checkFilter() {
      val l: Int = filter.length
      if (l % 2 == 0) {
        throw new IllegalArgumentException("filter length must be odd")
      }
    }
    checkFilter
    val fHalf = (filter.length - 1) / 2



    def filter(data: DenseVector[Double]): DenseVector[Double] = {
      val s:Int = data.length
      val myArray = DenseVector.zeros[Double](s)
      val x = data.length - filter.length
      var i = 0
      while (i <= x) {
        myArray(i + fHalf) = (data(i until i+filter.length) dot  filter )
        i += 1
      }
      return myArray
    }


  }

}

object JustRunConvolutionBenchmark {
  def main(args: Array[String]) {
    (new ConvolutionBenchmark).timeBreeze(100)
  }

}


object ConvolutionBenchmark {
  def main(args: Array[String]) {
    Runner.main(classOf[ConvolutionBenchmark], args)
  }
}