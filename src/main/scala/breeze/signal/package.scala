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

<<<<<<< HEAD
import breeze.signal.support.{OptOverhang, CanConvolve, CanFFT, CanIFFT}
=======
import breeze.signal.support.{OverhangOpt, CanConvolve}
>>>>>>> 54f3ecb2535a73b2b503cea533b198c8ec39df7f

/**This package provides digital signal processing functions.
 *
 * @author ktakagaki
 */
package object signal {

  // <editor-fold desc="Fourier Transforms">
  @deprecated("use fourierTransform", "v.0.6")
  val fft:fourierTransform.type = fourierTransform
  @deprecated("use inverseFourierTransform", "v.0.6")
  val ifft:inverseFourierTransform.type = inverseFourierTransform

  /**Convolves to DenseVectors or DenseMatrixes.</p>
    * Implementation is via the implicit trait CanConvolve[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanConvolve.scala.
    *
    * @param kernel DenseVector or DenseMatrix kernel
    * @param data DenseVector or DenseMatrix to be convolved
    * @param canConvolve implicit delegate which is used for implementation. End-users should not use this argument.
    * @return
    */
  def convolve[Input, Output](kernel: Input, data: Input, optOverhang: OptOverhang = OptOverhang.Default())
                             (implicit canConvolve: CanConvolve[Input, Output]): Output =
    canConvolve(kernel, data, optOverhang)

  // </editor-fold>

}
