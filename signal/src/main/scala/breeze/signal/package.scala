package breeze

import breeze.linalg._
import breeze.math.Complex
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D

/**
 * Created with IntelliJ IDEA.
 * User: takagaki
 * Date: 05.05.13
 * Time: 03:03
 * To change this template use File | Settings | File Templates.
 */

package object signal {

  /**
   *
   * @param v
   * @return
   */
  def fft(v: DenseVector[Double]): DenseVector[Complex] = {
    val fft_instance = new DoubleFFT_1D(v.size)

    //reformat for input
    val tempArr = new Array[Double](v.length*2)
    for (n <- 0 to v.length) {
      tempArr(n*2) = v.data(n)
      //tempIn(n*2 + 1) = 0  ...not necessary, arrays initialized to zero
    }

    //actual action
    fft_instance.realForwardFull( tempArr ) //does operation in place

    //reformat for output
    val tempRet: DenseVector[Complex] = DenseVector.zeros[Complex](v.length)
    for (n <- 0 to tempArr.length/ 2 -1) tempRet(n) = new Complex( tempArr(2*n), tempArr(2*n+1))

    tempRet

  }

}
