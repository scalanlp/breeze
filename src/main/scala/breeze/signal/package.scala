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

import breeze.signal._
import breeze.signal.support._


/**This package provides digital signal processing functions.
 *
 * @author ktakagaki
 */
package object signal {

  @deprecated("use fourierTransform", "v.0.6")
  val fft:fourierTransform.type = fourierTransform
  @deprecated("use inverseFourierTransform", "v.0.6")
  val ifft:inverseFourierTransform.type = inverseFourierTransform

  // <editor-fold desc="convolve, correlate">
  /**Convolves DenseVectors.</p>
    * Implementation is via the implicit trait CanConvolve[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanConvolve.scala.
    *
    * @param kernel DenseVector or DenseMatrix kernel
    * @param data DenseVector or DenseMatrix to be convolved
    * @param canConvolve implicit delegate which is used for implementation. End-users should not use this argument.
    */
  def convolve[Input, Output](data: Input, kernel: Input,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Value(0d),
                              method: OptMethod = OptMethod.Automatic
                              )
                             (implicit canConvolve: CanConvolve[Input, Output]): Output =
    canConvolve(data, kernel, correlate=false, overhang, padding, method)

  /**Correlates DenseVectors.</p>
    * Implementation is via the implicit trait CanConvolve[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanConvolve.scala.
    * See [[breeze.signal.convolve]] for options and other information.
    */
  def correlate[Input, Output](data: Input, kernel: Input,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Value(0d),
                              method: OptMethod = OptMethod.Automatic
                               )
                             (implicit canConvolve: CanConvolve[Input, Output]): Output =
    canConvolve(data, kernel, correlate=true, overhang, padding, method)

  // </editor-fold>

  /** Filter input data with the specified kernel and options.
    *
    * @param data data to be filtered
    * @param kernel filter kernel (argument of DenseVector[Double] will specify a FIR kernel with specified values).
    * @param overhang  whether to have overhanging values. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values. See [[breeze.signal.OptPadding]]
    * @param canFilter  (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filter[Input, Kernel, Output](data: Input, kernel: Kernel,
                            overhang: OptOverhang = OptOverhang.None,
                            padding: OptPadding = OptPadding.Boundary)
        (implicit canFilter: CanFilter[Input, Kernel, Output]): Output =
    canFilter(data, kernel, overhang, padding)

  /** Bandpass filter the input data.
    *
    * @param data data to be filtered
    * @param omega sequence of two filter band parameters, in units of the nyquist frequency,
    *              or in Hz if the sampleRate is set to a specific value other than 2d.
    * @param sampleRate default of 2.0 means that the nyquist frequency is 1.0
    * @param numtaps  number of taps to use (default = 512)
    * @param kernelType  currently only supports OptKernelType.Firwin. See [[breeze.signal.OptKernelType]]
    * @param overhang  whether to have overhanging values. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values. See [[breeze.signal.OptPadding]]
    * @param canFilterBPBS (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filterBandpass[Input, Output](data: Input, omega: (Double, Double),
                             numtaps: Int = 512,
                             sampleRate: Double = 2d,
                             kernelType: OptKernelType = OptKernelType.Firwin,
                             overhang: OptOverhang = OptOverhang.None,
                             padding: OptPadding = OptPadding.Boundary)
        (implicit canFilterBPBS: CanFilterBPBS[Input, Output]): Output =
    canFilterBPBS(data, omega,
              numtaps, sampleRate, bandStop = false,
              kernelType, overhang, padding)

  /** Bandstop filter the input data.
    *
    * @param data data to be filtered
    * @param omega sequence of two filter band parameters, in units of the nyquist frequency,
    *              or in Hz if the sampleRate is set to a specific value other than 2d.
    * @param sampleRate default of 2.0 means that the nyquist frequency is 1.0
    * @param numtaps  number of taps to use (default = 512)
    * @param kernelType  currently only supports OptKernelType.Firwin. See [[breeze.signal.OptKernelType]]
    * @param overhang  whether to have overhanging values. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values. See [[breeze.signal.OptPadding]]
    * @param canFilterBPBS (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filterBandstop[Input, Output](data: Input, omega: (Double, Double),
                                    numtaps: Int = 512,
                                    sampleRate: Double = 2d,
                                    kernelType: OptKernelType = OptKernelType.Firwin,
                                    overhang: OptOverhang = OptOverhang.None,
                                    padding: OptPadding = OptPadding.Boundary)
                                   (implicit canFilterBPBS: CanFilterBPBS[Input, Output]): Output =
    canFilterBPBS(data, omega,
      numtaps, sampleRate, bandStop = true,
      kernelType, overhang, padding)

}
