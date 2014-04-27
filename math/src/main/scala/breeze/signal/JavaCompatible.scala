package breeze.signal

import breeze.math.Complex
import breeze.util.JavaArrayOps._

/**This class is a converter for using breeze.signal functions on Arrays of Double and Complex, from Java/Matlab/Mathematica.
  *
 * @author ktakagaki
 * @date 3/11/14.
 */
object JavaCompatible {


  def convolve(data: Array[Double], kernel: Array[Double]) = dvDToArray( breeze.signal.convolve(arrayDToDv(data), arrayDToDv(kernel)) )
  def correlate(data: Array[Double], kernel: Array[Double]) = dvDToArray( breeze.signal.correlate(arrayDToDv(data), arrayDToDv(kernel)) )

  // <editor-fold defaultstate="collapsed" desc=" discrete Fourier transforms ">

  /**Returns the discrete fourier transform.
   * Use fourierTrC instead for complex array imput.
   * Use fourierTr2/2C instead for 2D Fourier tranform.
   *
   *
   * @return
   * @author ktakagaki, dlwh
   */
  def fourierTrD(data: Array[Double]): Array[Complex] = dvCToArray(breeze.signal.fourierTr( arrayDToDv(data) ))
  /**See [[fourierTrD]]*/
  def fourierTrC(data: Array[Complex]): Array[Complex] = dvCToArray(breeze.signal.fourierTr( arrayCToDv(data) ))
  /**See [[fourierTrD]]*/
  def iFourierTrC(data: Array[Complex]): Array[Complex] = dvCToArray(breeze.signal.iFourierTr( arrayCToDv(data) ))
  /**See [[fourierTrD]]*/
  def fourierTr2C(data: Array[Array[Complex]]): Array[Array[Complex]] = dmCToArray2( breeze.signal.fourierTr( array2CToDm(data) ) )

  // </editor-fold>
  // <editor-fold defaultstate="collapsed" desc=" Fourier transform related convenience functions ">

  /**Shift the zero-frequency component to the center of the spectrum.
    * Use fourierShiftC instead for complex array input.
    * This function swaps half-spaces for all axes listed (defaults to all). Note that y[0] is the Nyquist component only if len(x) is even.
    *
    * @param data input array
    * @return
    */
  def fourierShiftD(data: Array[Double]): Array[Double] = dvDToArray( breeze.signal.fourierShift( arrayDToDv(data) ) )
  /**See [[fourierShiftD]]*/
  def fourierShiftC(data: Array[Complex]): Array[Complex] = dvCToArray( breeze.signal.fourierShift( arrayCToDv(data) ) )

  /**Shift the zero-frequency component to the center of the spectrum.
    * Use fourierShiftC instead for complex array input.
    * This function swaps half-spaces for all axes listed (defaults to all). Note that y[0] is the Nyquist component only if len(x) is even.
    *
    * @param data input array
    * @return
    */
  def iFourierShiftD(data: Array[Double]): Array[Double] = dvDToArray( breeze.signal.iFourierShift( arrayDToDv( data ) ) )
  /**See [[iFourierShiftD]]*/
  def iFourierShiftC(data: Array[Complex]): Array[Complex] = dvCToArray( breeze.signal.iFourierShift( arrayCToDv(data) ) )

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
  def fourierFreqD(windowLength: Int, fs: Double, shifted: Boolean): Array[Double] =
        dvDToArray( breeze.signal.fourierFreq( windowLength, fs, -1, shifted) )
  /**See [[fourierFreq]]. shifted = false
    */
  def fourierFreqD(windowLength: Int, fs: Double): Array[Double] = fourierFreqD(windowLength, fs, false)

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" (FIR) filters ">
  /**Bandpass filter the data using a windowed FIR filter.
   * See/use [[breeze.signal.filterBP()]] for more details, and to set advanced options.
   *
   * @param data data to filter
   * @param omegaLow  low frequency (in units of Nyquist frequency or Hz if sampleRate is set to specific value other than 2d)
   * @param omegaHigh high frequency (in units of Nyquist frequency or Hz if sampleRate is set to specific value other than 2d)
   * @param sampleRate  in Hz, default 2d (omegaLow/High will then be in units of Nyquist frequency)
   * @param taps  number of taps to use, default 512
   * @return
   */
  def filterBP(data: Array[Double], omegaLow: Double, omegaHigh: Double, sampleRate: Double, taps: Int): Array[Double] =
    dvDToArray( breeze.signal.filterBP(arrayDToDv(data), (omegaLow, omegaHigh), sampleRate, taps) )
  /**See [[filterBP]]
    */
  def filterBP(data: Array[Double], omegaLow: Double, omegaHigh: Double, sampleRate: Double): Array[Double] = filterBP(data, omegaLow, omegaHigh, sampleRate, 512)
  /**See [[filterBP]]
    */
  def filterBP(data: Array[Double], omegaLow: Double, omegaHigh: Double): Array[Double] = filterBP(data, omegaLow, omegaHigh, 2d, 512)

  /**Bandstop filter the data using a windowed FIR filter.
    * See/use [[breeze.signal.filterBS()]] for more details, and to set advanced options.
    *
    * @param data data to filter
    * @param omegaLow  low frequency (in units of Nyquist frequency or Hz if sampleRate is set to specific value other than 2d)
    * @param omegaHigh high frequency (in units of Nyquist frequency or Hz if sampleRate is set to specific value other than 2d)
    * @param sampleRate  in Hz, default 2d (omegaLow/High will then be in units of Nyquist frequency)
    * @param taps  number of taps to use, default 512
    * @return
    */
  def filterBS(data: Array[Double], omegaLow: Double, omegaHigh: Double, sampleRate: Double, taps: Int): Array[Double] =
    dvDToArray( breeze.signal.filterBS(arrayDToDv(data), (omegaLow, omegaHigh), sampleRate, taps) )
  /**See [[filterBS]]
    */
  def filterBS(data: Array[Double], omegaLow: Double, omegaHigh: Double, sampleRate: Double): Array[Double] = filterBS(data, omegaLow, omegaHigh, sampleRate, 512)
  /**See [[filterBS]]
    */
  def filterBS(data: Array[Double], omegaLow: Double, omegaHigh: Double): Array[Double] = filterBS(data, omegaLow, omegaHigh, 2d, 512)

  /**Low pass filter the data using a windowed FIR filter.
    * See/use [[breeze.signal.filterLP()]] for more details, and to set advanced options.
    *
    * @param data data to filter
    * @param omega  cutoff frequency (in units of Nyquist frequency or Hz if sampleRate is set to specific value other than 2d)
    * @param sampleRate  in Hz, default 2d (omega will then be in units of Nyquist frequency)
    * @param taps  number of taps to use, default 512
    * @return
    */
  def filterLP(data: Array[Double], omega: Double, sampleRate: Double, taps: Int): Array[Double] =
    dvDToArray( breeze.signal.filterLP(arrayDToDv(data), omega, sampleRate, taps) )
  /**See [[filterLP]]
    */
  def filterLP(data: Array[Double], omega: Double, sampleRate: Double): Array[Double] = filterLP(data, omega, sampleRate, 512)
  /**See [[filterLP]]
    */
  def filterLP(data: Array[Double], omega: Double): Array[Double] = filterLP(data, omega, 2d, 512)

  /**High pass filter the data using a windowed FIR filter.
    * See/use [[breeze.signal.filterHP()]] for more details, and to set advanced options.
    *
    * @param data data to filter
    * @param omega  cutoff frequency (in units of Nyquist frequency or Hz if sampleRate is set to specific value other than 2d)
    * @param sampleRate  in Hz, default 2d (omega will then be in units of Nyquist frequency)
    * @param taps  number of taps to use, default 512
    * @return
    */
  def filterHP(data: Array[Double], omega: Double, sampleRate: Double, taps: Int): Array[Double] =
    dvDToArray( breeze.signal.filterHP(arrayDToDv(data), omega, sampleRate, taps) )
  /**See [[filterHP]]
    */
  def filterHP(data: Array[Double], omega: Double, sampleRate: Double): Array[Double] = filterHP(data, omega, sampleRate, 512)
  /**See [[filterHP]]
    */
  def filterHP(data: Array[Double], omega: Double): Array[Double] = filterHP(data, omega, 2d, 512)

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" wavelets ">

  /**Return the padded fast haar transformation of a vector or matrix. Note that
    * the output will always be padded to a power of 2.</p>
    * A matrix will cause a 2D fht. The 2D haar transformation is defined for squared power of 2
    * matrices. A new matrix will thus be created and the old matrix will be placed in the upper-left
    * part of the new matrix. Avoid calling this method with a matrix that has few cols / many rows or
    * many cols / few rows (e.g. 1000000 x 3) as this will cause a very high memory consumption.
    *
    * @see https://en.wikipedia.org/wiki/Haar_wavelet
    * @param data data to be transformed.
    */
  def haarTrD( data: Array[Double] ) = dvDToArray( breeze.signal.haarTr( arrayDToDv(data) ) )
  /**See [[haarTrD]]
    */
  def haarTr2D( data: Array[Array[Double]] ) = dmDToArray2( breeze.signal.haarTr( array2DToDm(data) ) )

  // </editor-fold>

  /**Root mean square of a vector.*/
  def rootMeanSquareD( data: Array[Double] ): Double = breeze.signal.rootMeanSquare( arrayDToDv(data) )



  /** Median filter the input data. Edge values are median-filtered with shorter windows, in order to preserve
    * the total length of the input.
    * @param windowLength only supports odd windowLength values, since even values would cause half-frame time shifts in one or the other direction,
    *                     and would also lead to floating point values even for integer input
    */
  def filterMedianD(data: Array[Double], windowLength: Int) = dvDToArray( breeze.signal.filterMedian( arrayDToDv(data), windowLength ) )




}
