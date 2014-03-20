package breeze.signal

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.Complex
import breeze.signal.JavaArrayOps._

/**This class is a converter for using breeze.signal functions on Arrays of Double and Complex, from Java/Matlab/Mathematica.
  *
 * @author ktakagaki
 * @date 3/11/14.
 */
object JavaCompatible {


  def convolve(data: Array[Double], kernel: Array[Double]) = breeze.signal.convolve(DenseVector(data), DenseVector(kernel)).toArray

  /**Returns the discrete fourier transform.
   * Use fourierTrC instead for complex array imput.
   * Use fourierTr2/2C instead for 2D Fourier tranform.
   *
   *
   * @return
   * @author ktakagaki, dlwh
   */
  def fourierTrD(data: Array[Double]): Array[Complex] = denseVectorCToArray(breeze.signal.fourierTr( DenseVector(data) ))
  /**See [[fourierTrD]]*/
  def fourierTrC(data: Array[Complex]): Array[Complex] = denseVectorCToArray(breeze.signal.fourierTr( DenseVector(data) ))
  /**See [[fourierTrD]]*/
  def iFourierTrC(data: Array[Complex]): Array[Complex] = denseVectorCToArray(breeze.signal.iFourierTr( DenseVector(data) ))
  /**See [[fourierTrD]]*/
  def fourierTr2C(data: Array[Array[Complex]]): Array[Array[Complex]] = denseMatrixCToArray2( breeze.signal.fourierTr( array2CToDenseMatrix(data) ) )

  /**Shift the zero-frequency component to the center of the spectrum.
    * Use fourierShiftC instead for complex array input.
    * This function swaps half-spaces for all axes listed (defaults to all). Note that y[0] is the Nyquist component only if len(x) is even.
    *
    * @param data input array
    * @return
    */
  def fourierShiftD(data: Array[Double]): Array[Double] = denseVectorDToArray( breeze.signal.fourierShift( arrayDToDenseVector(data) ) )
  /**See [[fourierShiftD]]*/
  def fourierShiftC(data: Array[Complex]): Array[Complex] = denseVectorCToArray( breeze.signal.fourierShift( arrayCToDenseVector(data) ) )

  /**Shift the zero-frequency component to the center of the spectrum.
    * Use fourierShiftC instead for complex array input.
    * This function swaps half-spaces for all axes listed (defaults to all). Note that y[0] is the Nyquist component only if len(x) is even.
    *
    * @param data input array
    * @return
    */
  def iFourierShiftD(data: Array[Double]): Array[Double] = denseVectorDToArray( breeze.signal.iFourierShift( arrayDToDenseVector( data ) ) )
  /**See [[iFourierShiftD]]*/
  def iFourierShiftC(data: Array[Complex]): Array[Complex] = denseVectorCToArray( breeze.signal.iFourierShift( arrayCToDenseVector(data) ) )

  /**Returns the frequencies for each tap in a discrete Fourier transform, useful for plotting.
    * You must specify either an fs or a dt argument. If you specify both, which is redundant,
    * fs == 1.0/dt must be true.
    *
    * f = [0, 1, ..., n/2-1, -n/2, ..., -1] / (dt*n)         if n is even
    * f = [0, 1, ..., (n-1)/2, -(n-1)/2, ..., -1] / (dt*n)   if n is odd
    *
    * @param windowLength window length of discrete Fourier transform
    * @param fs  sampling frequency (Hz)
    * @param shifted whether to return fourierShift'ed frequencies, default=false
    */
  def fourierFreq(windowLength: Int, fs: Double, shifted: Boolean): Array[Double] =
        denseVectorDToArray( breeze.signal.fourierFreq( windowLength, fs, -1, shifted) )

  /**See [[fourierFreq]]. shifted = false
    */
  def fourierFreq(windowLength: Int, fs: Double): Array[Double] = fourierFreq(windowLength, fs, false)


  /** Median filter the input data. Edge values are median-filtered with shorter windows, in order to preserve
    * the total length of the input.
    * @param windowLength only supports odd windowLength values, since even values would cause half-frame time shifts in one or the other direction,
    *                     and would also lead to floating point values even for integer input
    */
  def filterMedian(data: Array[Double], windowLength: Int) = denseVectorDToArray( breeze.signal.filterMedian( arrayDToDenseVector(data), windowLength ) )



}
