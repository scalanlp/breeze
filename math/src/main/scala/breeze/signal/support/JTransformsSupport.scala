package breeze.signal.support

import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D
import breeze.linalg.DenseVector
import breeze.math.Complex

/** This class encapsulates convenience methods to use the JTransforms package.
 *
 * Created with IntelliJ IDEA.
 * User: takagaki
 * Date: 25.06.13
 * Time: 21:02
 * To change this template use File | Settings | File Templates.
 */
object JTransformsSupport {

  //maintain instance of transform to eliminate repeated initialization
  private var fft_instD1D: (Int, DoubleFFT_1D) = (0, null)
  def getD1DInstance(length: Int): DoubleFFT_1D = {
    if(length == fft_instD1D._1) fft_instD1D._2
    else {
      fft_instD1D = (length, new DoubleFFT_1D(length))
      fft_instD1D._2
    }
  }

  def tempToDenseVector(tempArr: Array[Double]): DenseVector[Complex] = {
    val tempRet = DenseVector.zeros[Complex](tempArr.length/2)
    for (n <- 0 until tempRet.length) tempRet(n) = new Complex( tempArr(2*n), tempArr(2*n+1))
    tempRet
  }

  /**
   * Reformat for input: note difference in format with denseVectorDToTemp
   * @param tempDV
   * @return
   */
  def denseVectorCToTemp(tempDV: DenseVector[Complex]): Array[Double] = {
    val tempRet = new Array[Double](tempDV.length*2)
    for(n <- 0 until tempDV.length) {
      tempDV(n) match {
        case Complex(re, im) => {
          tempRet(2*n) = re
          tempRet(2*n+1) = im
        }
      }
    }
    tempRet
  }

  /**
   * Reformat for input: note difference in format with denseVectorCToTemp
   * @param tempDV
   * @return
   */
  def denseVectorDToTemp(tempDV: DenseVector[Double]): Array[Double] = {
    val tempArr = new Array[Double](tempDV.length*2)
    for(n <- 0 until tempDV.length) tempArr(n) = tempDV(n)
    tempArr
  }

}
