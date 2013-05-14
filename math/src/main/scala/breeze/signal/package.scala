package breeze
/*
Copyright 2012 David Hall

Licensed under the Apache License, Version 2.0 (the "License")
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

import breeze.linalg._
import breeze.math.Complex
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D

/**This package provides digital signal processing functions.
 *
 * @author ktakagaki
 */
package object signal {

  //maintain instances of transform to eliminate repeated initialization
  private var fft_instD1D: (Int, DoubleFFT_1D) = (0, null)
  private def getD1DInstance(length: Int): DoubleFFT_1D = {
    if(length == fft_instD1D._1) fft_instD1D._2
    else {
      fft_instD1D = (length, new DoubleFFT_1D(length))
      fft_instD1D._2
    }
  }

  private def tempToDenseVector(tempArr: Array[Double]): DenseVector[Complex] = {
    val tempRet = DenseVector.zeros[Complex](tempArr.length/2)
    for (n <- 0 until tempRet.length) tempRet(n) = new Complex( tempArr(2*n), tempArr(2*n+1))
    tempRet
  }
  private def denseVectorCToTemp(tempDV: DenseVector[Complex]): Array[Double] = {
    val tempRet = Array[Double](tempDV.length*2)
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
   * Computes 1D forward DFT
   *
   * @param v data to transform
   */
  def fft(v: DenseVector[Double]): DenseVector[Complex] = {
    val fft_instance = getD1DInstance(v.length)

    //reformat for input: note difference in format for input to complex fft
    val tempArr = DenseVector.zeros[Double](v.length*2)
    tempArr(0 until v.length) := v
    //actual action
    fft_instance.realForwardFull( tempArr.data ) //does operation in place
    //reformat for output
    tempToDenseVector(tempArr)
  }

  /**
   * Computes 1D forward DFT
   *
   * @param v data to transform
   */
  def fft(v: DenseVector[Complex]): DenseVector[Complex] = {
    val fft_instance = getD1DInstance(v.length)

    //reformat for input: note difference in format for input to real fft
    val tempArr = denseVectorCToTemp(v)
    //actual action
    fft_instance.complexForward( tempArr ) //does operation in place
    //reformat for output
    tempToDenseVector(tempArr)
  }

  /**
   * Computes 1D inverse DFT
   *
   * @param v data to transform
   */
  def ifft(v: DenseVector[Complex]): DenseVector[Complex] = {
    val fft_instance = getD1DInstance(v.length)

    //reformat for input: note difference in format for input to real fft
    val tempArr = denseVectorCToTemp(v)
    //actual action
    fft_instance.complexInverse( tempArr, true ) //does operation in place
    //reformat for output
    tempToDenseVector(tempArr)
  }

  /**
   * Computes 1D inverse DFT
   *
   * @param v data to transform
   */
  def ifft(v: DenseVector[Complex]): DenseVector[Complex] = {
    val fft_instance = getD1DInstance(v.length)

    //reformat for input: note difference in format for input to real fft
    val tempArr = denseVectorCToTemp(v)
    //actual action
    fft_instance.complexInverse( tempArr, true ) //does operation in place
    //reformat for output
    tempToDenseVector(tempArr)
  }

}
