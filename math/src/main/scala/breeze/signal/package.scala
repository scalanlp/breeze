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
import breeze.linalg.DenseVector
import breeze.numerics.isEven
import breeze.macros.expand

/**This package provides digital signal processing functions.
 *
 * @author ktakagaki
 */
package object signal {

  @deprecated("use fourierTr", "v.0.6")
  val fft: fourierTr.type = fourierTr
  @deprecated("use iFourierTr", "v.0.6")
  val ifft: iFourierTr.type = iFourierTr
  @deprecated("use fourierTr", "v.0.6")
  val fourierTransform: fourierTr.type = fourierTr
  @deprecated("use iFourierTr", "v.0.6")
  val inverseFourierTransform: iFourierTr.type = iFourierTr

  // <editor-fold desc="fourierFreq">

  /**Returns the frequencies for each tap in a discrete Fourier transform, useful for plotting.
    * You must specify either an fs or a dt argument. If you specify both, which is redundant,
    * fs == 1.0/dt must be true.
    *
    * f = [0, 1, ..., n/2-1, -n/2, ..., -1] / (dt*n)         if n is even
    * f = [0, 1, ..., (n-1)/2, -(n-1)/2, ..., -1] / (dt*n)   if n is odd
   *
   * @param windowLength window length of discrete Fourier transform
   * @param fs  sampling frequency (1.0/dt; specify default of -1 if using dt)
   * @param dt  time step (CAUTION: 1.0/fs; specify default of -1 if using fs)
   * @param shifted whether to return fourierShift'ed frequencies, default=false
   */
  def fourierFreq(windowLength: Int, fs: Double = -1, dt: Double = -1, shifted: Boolean = false ): DenseVector[Double] = {
    require(fs>0 || dt>0, "Must specify either a valid fs or a valid dt argument.")
    if(fs>0 && dt>0) require(fs == 1d/dt, "If fs and dt are both specified, fs == 1.0/dt must be true. Otherwise, they are incompatible")
    val realFs = if(fs<0 && dt>0) 1d/dt else fs

    val shiftedFreq = if( isEven(windowLength) ){
      DenseVector.vertcat(
        DenseVector.tabulate(0 to windowLength/2 - 1)( (i: Int) => i.toDouble*realFs/windowLength.toDouble ),
        DenseVector.tabulate(- windowLength/2 to - 1)( (i: Int) => i.toDouble*realFs/windowLength.toDouble )
      )
    } else {
      DenseVector.vertcat(
        DenseVector.tabulate(0 to (windowLength-1)/2 )( (i: Int) => i.toDouble*realFs/windowLength.toDouble ),
        DenseVector.tabulate(- (windowLength-1)/2 to -1 )( (i: Int) => i.toDouble*realFs/windowLength.toDouble )
      )
    }
    if(shifted) fourierShift(shiftedFreq) else shiftedFreq
  }


  // </editor-fold>

  // <editor-fold desc="convolve, correlate">
  /**Convolves DenseVectors.</p>
    * Implementation is via the implicit trait CanConvolve[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanConvolve.scala.
    *
    * @param kernel DenseVector or DenseMatrix kernel
    * @param data DenseVector or DenseMatrix to be convolved
    * @param canConvolve implicit delegate which is used for implementation. End-users should not use this argument.
    */
  def convolve[Input, KernelType, Output](
                              data: Input, kernel: KernelType, range: OptRange = OptRange.All,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Zero,
                              method: OptMethod = OptMethod.Automatic
                              )
                             (implicit canConvolve: CanConvolve[Input, KernelType, Output]): Output =
    canConvolve(data, kernel, range, correlate=false, overhang, padding, method)

  /**Correlates DenseVectors.</p>
    * Implementation is via the implicit trait CanConvolve[ InputType,  OutputType ],
    * which is found in breeze.signal.support.CanConvolve.scala.
    * See [[breeze.signal.convolve]] for options and other information.
    */
  def correlate[Input, KernelType, Output](
                              data: Input, kernel: KernelType, range: OptRange = OptRange.All,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Zero,
                              method: OptMethod = OptMethod.Automatic
                               )
                             (implicit canConvolve: CanConvolve[Input, KernelType, Output]): Output =
    canConvolve(data, kernel, range, correlate=true, overhang, padding, method)

  // </editor-fold>

  // <editor-fold desc="filter">
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
                            overhang: OptOverhang = OptOverhang.PreserveLength,
                            padding: OptPadding = OptPadding.Zero)
        (implicit canFilter: CanFilter[Input, Kernel, Output]): Output =
    canFilter(data, kernel, overhang, padding)

  // </editor-fold>

  // <editor-fold desc="filterBP, filterBS">

  /** Bandpass filter the input data.
    *
    * @param data data to be filtered
    * @param taps  number of taps to use (default = 512)
    * @param omegas sequence of two filter band parameters, in units of the Nyquist frequency,
    *              or in Hz if the sampleRate is set to a specific value other than 2d.
    * @param sampleRate default of 2.0 means that the Nyquist frequency is 1.0
    * @param kernelDesign  currently only supports OptKernelType.Firwin. See [[breeze.signal.OptDesignMethod]]
    * @param overhang  whether to have overhanging values when filtering. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values when filtering. See [[breeze.signal.OptPadding]]
    * @param canFilterBPBS (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filterBP[Input, Output](data: Input,
                              omegas: (Double, Double),
                              sampleRate: Double = 2d,
                              taps: Int = 512,
                              kernelDesign: OptDesignMethod = OptDesignMethod.Firwin,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Boundary)
        (implicit canFilterBPBS: CanFilterBPBS[Input, Output]): Output =
    canFilterBPBS(data, omegas, sampleRate, taps, bandStop = false,
              kernelDesign, overhang, padding)

  /** Bandstop filter the input data.
    *
    * @param data data to be filtered
    * @param taps  number of taps to use (default = 512)
    * @param omegas sequence of two filter band parameters, in units of the Nyquist frequency,
    *              or in Hz if the sampleRate is set to a specific value other than 2d.
    * @param sampleRate default of 2.0 means that the Nyquist frequency is 1.0
    * @param kernelDesign  currently only supports OptKernelType.Firwin. See [[breeze.signal.OptDesignMethod]]
    * @param overhang  whether to have overhanging values when filtering. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values when filtering. See [[breeze.signal.OptPadding]]
    * @param canFilterBPBS (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filterBS[Input, Output](data: Input,
                              omegas: (Double, Double),
                              sampleRate: Double = 2d,
                              taps: Int = 512,
                              kernelDesign: OptDesignMethod = OptDesignMethod.Firwin,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Boundary)
         (implicit canFilterBPBS: CanFilterBPBS[Input, Output]): Output =
    canFilterBPBS(data, omegas, sampleRate, taps,bandStop = true,
      kernelDesign, overhang, padding)

  // </editor-fold>

  // <editor-fold desc="filterLP, filterHP">

  /** Lowpass filter the input data.
    *
    * @param data data to be filtered
    * @param taps  number of taps to use (default = 512)
    * @param omega cutoff frequency, in units of the Nyquist frequency,
    *              or in Hz if the sampleRate is set to a specific value other than 2d.
    * @param sampleRate default of 2.0 means that the Nyquist frequency is 1.0
    * @param kernelDesign  currently only supports OptKernelType.Firwin. See [[breeze.signal.OptDesignMethod]]
    * @param overhang  whether to have overhanging valueswhen filtering. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values when filtering. See [[breeze.signal.OptPadding]]
    * @param canFilterLPHP (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filterLP[Input, Output](data: Input,
                              omega: Double,
                              sampleRate: Double = 2d,
                              taps: Int = 512,
                              kernelDesign: OptDesignMethod = OptDesignMethod.Firwin,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Boundary)
                             (implicit canFilterLPHP: CanFilterLPHP[Input, Output]): Output =
    canFilterLPHP(data, omega, sampleRate, taps, lowPass = true, kernelDesign, overhang, padding)

  /** Highpass filter the input data.
    *
    * @param data data to be filtered
    * @param taps  number of taps to use (default = 512)
    * @param omega cutoff frequency, in units of the Nyquist frequency,
    *              or in Hz if the sampleRate is set to a specific value other than 2d.
    * @param sampleRate default of 2.0 means that the Nyquist frequency is 1.0
    * @param kernelDesign  currently only supports OptKernelType.Firwin. See [[breeze.signal.OptDesignMethod]]
    * @param overhang  whether to have overhanging values when filtering. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values when filtering. See [[breeze.signal.OptPadding]]
    * @param canFilterLPHP (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filterHP[Input, Output](data: Input,
                              omega: Double,
                              sampleRate: Double = 2d,
                              taps: Int = 512,
                              kernelDesign: OptDesignMethod = OptDesignMethod.Firwin,
                              overhang: OptOverhang = OptOverhang.None,
                              padding: OptPadding = OptPadding.Boundary)
                             (implicit canFilterLPHP: CanFilterLPHP[Input, Output]): Output =
    canFilterLPHP(data, omega, sampleRate, taps, lowPass = false, kernelDesign, overhang, padding)

  // </editor-fold>

  // <editor-fold desc="filter design">

  /** FIR filter design using the window method.
    *
    * This function computes the coefficients of a finite impulse response
    * filter.  The filter will have linear phase; it will be Type I if
    * `numtaps` is odd and Type II if `numtaps` is even.
    *
    * Type II filters always have zero response at the Nyquist rate, so a
    *  ValueError exception is raised if firwin is called with `numtaps` even and
    * having a passband whose right end is at the Nyquist rate.
    *
    *  Portions of the code are translated from scipy (scipy.org) based on provisions of the BSD license.
    *
    * @param omegas Cutoff frequencies of the filter, specified in units of "nyquist."
    *               The frequencies should all be positive and monotonically increasing.
    *               The frequencies must lie between (0, nyquist).
    *               0 and nyquist should not be included in this array.
    *
    * @param optWindow Currently supports a hamming window [[breeze.signal.OptWindowFunction.Hamming]],
    *               a specified window [[breeze.signal.OptWindowFunction.User]], or
    *               no window [[breeze.signal.OptWindowFunction.None]].
    * @param zeroPass If true (default), the gain at frequency 0 (ie the "DC gain") is 1, if false, 0.
    * @param scale Whether to scale the coefficiency so that frequency response is unity at either (A) 0 if zeroPass is true
    *              or (B) at nyquist if the first passband ends at nyquist, or (C) the center of the first passband. Default is true.
    * @param nyquist The nyquist frequency, default is 1.
    */
  def designFilterFirwin[Output](taps: Int, omegas: DenseVector[Double], nyquist: Double = 1d,
                zeroPass: Boolean = true,
                scale: Boolean = true, multiplier: Double = 1d,
                optWindow: OptWindowFunction = OptWindowFunction.Hamming()  )
               (implicit canFirwin: CanFirwin[Output]): FIRKernel1D[Output] =
     canFirwin(taps, omegas, nyquist, zeroPass, scale, multiplier, optWindow)

  def designFilterDecimation[Output](factor: Int, multiplier: Double = 1d,
                                     optDesignMethod: OptDesignMethod = OptDesignMethod.Firwin,
                                     optWindow: OptWindowFunction = OptWindowFunction.Hamming(),
                                     optFilterOrder: OptFilterTaps = OptFilterTaps.Automatic)
                                (implicit canDesignFilterDecimation: CanDesignFilterDecimation[Output]): Output =
    canDesignFilterDecimation(factor, multiplier, optDesignMethod, optWindow, optFilterOrder)

  // </editor-fold>

//ToDo  def designFilterButterworth()
    
  // <editor-fold defaultstate="collapsed" desc=" filterMedian ">

  /** Median filter the input data.
    * @param windowLength only supports odd windowLength values, since even values would cause half-frame time shifts in one or the other direction,
    *                     and would also lead to floating point values even for integer input
    * @param overhang specify OptOverhang.PreserveLength (default) or OptOverhang.None (result will be (windowLength -1) shorter)
    *                 for OptOverhang.PreserveLength, the edges will feature symmetrical odd windows of increasing size,
    *                 ie ( median( {0} ), median( {0, 1, 2} ), median( {0, 1, 2, 3, 4} )... )
    */
  def filterMedian[Input](data: DenseVector[Input], windowLength: Int, overhang: OptOverhang = OptOverhang.PreserveLength)
                             (implicit canFilterMedian: CanFilterMedian[Input]): DenseVector[Input] =
    canFilterMedian(data, windowLength, overhang)
  def filterMedian[Input](data: DenseVector[Input], windowLength: Int)
                         (implicit canFilterMedian: CanFilterMedian[Input]): DenseVector[Input] =
    canFilterMedian(data, windowLength, OptOverhang.PreserveLength)


  // </editor-fold>

  /**Return the padded fast haar transformation of a DenseVector or DenseMatrix. Note that
   * the output will always be padded to a power of 2.</p>
   * A matrix will cause a 2D fht. The 2D haar transformation is defined for squared power of 2
   * matrices. A new matrix will thus be created and the old matrix will be placed in the upper-left
   * part of the new matrix. Avoid calling this method with a matrix that has few cols / many rows or
   * many cols / few rows (e.g. 1000000 x 3) as this will cause a very high memory consumption.
   *
   * @see https://en.wikipedia.org/wiki/Haar_wavelet
   * @param v DenseVector or DenseMatrix to be transformed.
   * @param canHaarTransform implicit delegate which is used for implementation. End-users should not use this argument.
   * @return DenseVector or DenseMatrix
   */
  def haarTr[Input, Output](v : Input)(implicit canHaarTransform: CanHaarTr[Input, Output]): Output =
    canHaarTransform(v)

  @deprecated("use haarTr", "v.0.6")
  def haarTransform[Input, Output](v : Input)(implicit canHaarTransform: CanHaarTr[Input, Output]): Output =
    canHaarTransform(v)

  /**Returns the inverse fast haar transform for a DenseVector or DenseMatrix.
   *
   */
  def iHaarTr[Input, Output](v : Input)
      (implicit canInverseHaarTransform: CanIHaarTr[Input, Output]): Output =
          canInverseHaarTransform(v)

  @deprecated("use iHaarTr", "v.0.6")
  def inverseHaarTransform[Input, Output](v : Input)
                            (implicit canInverseHaarTransform: CanIHaarTr[Input, Output]): Output =
    canInverseHaarTransform(v)

}
