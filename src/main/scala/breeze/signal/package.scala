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

import breeze.signal.support._
import breeze.signal.filter.{FIRKernel1D, FilterKernel}

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
  /**Convolves to DenseVectors or DenseMatrixes.</p>
    * Implementation is via the implicit trait CanConvolve[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanConvolve.scala.
    *
    * @param kernel DenseVector or DenseMatrix kernel
    * @param data DenseVector or DenseMatrix to be convolved
    * @param canConvolve implicit delegate which is used for implementation. End-users should not use this argument.
    * @return
    */
  def convolve[Input, Output](kernel: Input, data: Input,
                              cyclical: Boolean = true,
                              optOverhang: OptOverhang = OptOverhang.OptDefault(),
                              optMethod: OptMethod = OptMethod.OptAutomatic()
                              )
                             (implicit canConvolve: CanConvolve[Input, Output]): Output =
    canConvolve(kernel, data, cyclical, optOverhang, optMethod)

  // </editor-fold>

  def filter[Input, Kernel, Output](data: Input, kernel: Kernel,
                             optPadding: OptPadding = OptPadding.OptBoundary)
        (implicit canFilter: CanFilter[Input, Kernel, Output]): Output =
    canFilter(data, kernel, optPadding)

  def filterBandpass[Input, Output](data: Input, omega: (Double, Double),
                             sampleRate: Double = 1d,
                             optPadding: OptPadding = OptPadding.OptBoundary)
        (implicit canFilter: CanFilter[Input, FIRKernel1D, Output]): Output =
    canFilter(data, firwin(), optPadding)

}
