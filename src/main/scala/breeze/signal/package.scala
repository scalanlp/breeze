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

import breeze.signal.support.{OverhangOpt, CanConvolve, CanFFT, CanIFFT}

/**This package provides digital signal processing functions.
 *
 * @author ktakagaki
 */
package object signal {

  // <editor-fold desc="Fourier Transforms">
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
  def fourierTransform[Input, Output](v: Input)(implicit canFFT: CanFFT[Input, Output]): Output = canFFT(v)
  /**Deprecated, see [[breeze.signal.fourierTransform]]*/
  @deprecated("use fourierTransform", "v.0.6")
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
  def inverseFourierTransform[Input, Output](v: Input)(implicit canIFFT: CanIFFT[Input, Output]): Output = canIFFT(v)
  /**Deprecated, see [[breeze.signal.inverseFourierTransform]]*/
  @deprecated("use inverseFourierTransform", "v.0.6")
  def ifft[Input, Output](v: Input)(implicit canIFFT: CanIFFT[Input, Output]): Output = canIFFT(v)
  // </editor-fold>

  // <editor-fold desc="Convolution">

  /**Convolves to DenseVectors or DenseMatrixes.</p>
    * Implementation is via the implicit trait CanConvolve[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanConvolve.scala.
    *
    * @param kernel DenseVector or DenseMatrix kernel
    * @param data DenseVector or DenseMatrix to be convolved
    * @param canConvolve implicit delegate which is used for implementation. End-users should not use this argument.
    * @return
    */
  def convolve[Input, Output](kernel: Input, data: Input, overhangOpt: OverhangOpt = OverhangOpt.Default())
                             (implicit canConvolve: CanConvolve[Input, Output]): Output =
    canConvolve(kernel, data, overhangOpt)

  // </editor-fold>

}
