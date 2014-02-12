package breeze.signal

import org.scalatest._
import breeze.linalg.{DenseVector}

/**
 * Created with IntelliJ IDEA.
 * User: takagaki
 * Date: 14.05.13
 * Time: 02:31
 * To change this template use File | Settings | File Templates.
 */
class ConvolveTest extends FunSuite {

  test("convolve/correlate") {
    val kernelEven = DenseVector(1.0, 2.0)
    val kernelOdd = DenseVector(1.0, 2.0, 3.0)
    val data = DenseVector(2.0, 3.0, 4.0, 5.0)
    assert( convolve(data, kernelEven) == DenseVector(7.0, 10.0, 13.0) )
    assert( correlate(data, kernelEven) == DenseVector(8.0, 11.0, 14.0) )
    assert( convolve(data, kernelEven, overhang = OptOverhang.Full ) == DenseVector(2.0, 7.0, 10.0, 13.0, 10) )
    assert( correlate(data, kernelEven, overhang = OptOverhang.Full ) == DenseVector(4.0, 8.0, 11.0, 14.0, 5.0) )
    //println(convolve(data, kernel, overhang = OptOverhang.PreserveLength ))
    assert( convolve(data, kernelEven, overhang = OptOverhang.PreserveLength ) == DenseVector(7.0, 10.0, 13.0, 10.0) )
    assert( correlate(data, kernelEven, overhang = OptOverhang.PreserveLength ) == DenseVector(8.0, 11.0, 14.0, 5.0) )
    assert( convolve(data, kernelOdd, overhang = OptOverhang.PreserveLength ) == DenseVector(7.0, 16.0, 22.0, 22.0) )
    assert( correlate(data, kernelOdd, overhang = OptOverhang.PreserveLength ) == DenseVector(13.0, 20.0, 26.0, 14.0) )

    assert( convolve(data, kernelEven, 0 to 1 ) == DenseVector(7.0, 10.0) )
    assert( correlate(data, kernelEven, 2 to -1, overhang = OptOverhang.Full ) == DenseVector( 11.0, 14.0, 5.0 ) )

  }
  //MatLab: conv(2 : 5, 1 : 2)
  //Mathematica: ListConvolve[{1, 2}, {2, 3, 4, 5}]

}