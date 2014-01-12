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

import breeze.signal.support.{CanFFT, CanIFFT}
import breeze.signal.support.{CanHaarTransform, CanInverseHaarTransform}

/**This package provides digital signal processing functions.
 *
 * @author ktakagaki
 */
package object signal {

  /**Returns the fast fourier transform of a DenseVector or DenseMatrix. Currently,
   * DenseVector/DenseMatrix types of Double and Complex are supported. Scaling
   * follows the common signal processing convention, i.e. <b>no scaling on forward DFT</b>,
   * and 1/n scaling for the inverse DFT. Of note, fft(x: DenseMatrix[Double]) will
   * perform the 2D fft in both row and column dimensions, as opposed to the MatLab
   * toolbox syntax, which performs column-wise 1D fft.</p>
   * Implementation is via the implicit trait CanFFT[ InputType,  OutputType ],
   * which is found in breeze.signal.support.CanFFT.scala.
   *
   * @param v DenseVector or DenseMatrix to be fft'ed
   * @param canFFT implicit delegate which is used for implementation. End-users should not use this argument.
   * @return
   */
  def fft[Input, Output](v: Input)(implicit canFFT: CanFFT[Input, Output]): Output = canFFT(v)

  /**Returns the inverse fast fourier transform of a DenseVector or DenseMatrix. Currently,
    * DenseVector/DenseMatrix types of Double and Complex are supported. Scaling
    * follows the common signal processing convention, i.e. no scaling on forward DFT,
    * and <b>1/n scaling for the inverse DFT</b>. Of note, ifft(x: DenseMatrix[Double]) will
    * perform the 2D ifft in both row and column dimensions, as opposed to the MatLab
    * toolbox syntax, which performs column-wise 1D ifft.</p>
    * Implementation is via the implicit trait CanIFFT[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanIFFT.scala.
    *
    * @param v DenseVector or DenseMatrix to be fft'ed
    * @param canIFFT implicit delegate which is used for implementation. End-users should not use this argument.
    * @return
    */
  def ifft[Input, Output](v: Input)(implicit canIFFT: CanIFFT[Input, Output]): Output = canIFFT(v)

  /**Return the padded fast haar transformation of a DenseVector or DenseMatrix. Note that
   * the output will always be padded to a power of 2.</p>
   * A matrix will cause a 2D fht. The 2D haar transformation is defined for squared power of 2
   * matrices. A new matrix will thus be created and the old matrix will be placed in the upper-left
   * part of the new matrix. Avoid calling this method with a matrix that has few cols / many rows or
   * many cols / few rows (e.g. 1000000 x 3) as this will cause a very high memory consumption.
   *
   * @see https://en.wikipedia.org/wiki/Haar_wavelet
   * @param v DenseVector or DenseMatrix to be transformed.
   * @param canFHT implicit delegate which is used for implementation. End-users should not use this argument.
   * @return DenseVector or DenseMatrix
   */
  def haarTransform[Input, Output](v : Input)(implicit canHaarTransform: CanHaarTransform[Input, Output]): Output =
    canHaarTransform(v)

  /**Returns the inverse fast haar transform for a DenseVector or DenseMatrix.
   *
   */
  def inverseHaarTransform[Input, Output](v : Input)
      (implicit canInverseHaarTransform: CanInverseHaarTransform[Input, Output]): Output =
          canInverseHaarTransform(v)

}
