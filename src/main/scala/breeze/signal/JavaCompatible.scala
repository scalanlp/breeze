package breeze.signal

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.Complex

/**This class is a converter for using breeze.signal functions on Arrays of Double and Complex, from Java/Matlab/Mathematica.
  *
 * @author ktakagaki
 * @date 3/11/14.
 */
object JavaCompatible {

  class DenseVectorComplex( array: Array[Complex] ) extends DenseVector[Complex]( array )
  class DenseVectorDouble( array: Array[Double] ) extends DenseVector[Double]( array )
  class DenseVectorInt( array: Array[Int] ) extends DenseVector[Int]( array )

  class DenseMatrixComplex( array: Array[Array[Complex]] ) extends DenseMatrix[Complex]( array )
  class DenseMatrixDouble( array: Array[Array[Double]] ) extends DenseMatrix[Double]( array )
  class DenseMatrixInt( array: Array[Array[Int]] ) extends DenseMatrix[Int]( array )



  private def dvCToArray(data: DenseVector[Complex]): Array[Complex] = data.toArray

  def convolve(data: Array[Double], kernel: Array[Double]) = breeze.signal.convolve(DenseVector(data), DenseVector(kernel)).toArray

  /**Returns the discrete fourier transform.
   * Use fourierTrC instead for complex array imput.
   * Use fourierTr2/2C instead for 2D Fourier tranform.
   *
   *
   * @return
   * @author ktakagaki, dlwh
   */
  def fourierTrD(data: Array[Double]): Array[Complex] = dvCToArray(breeze.signal.fourierTr( DenseVector(data) ))
  /**See [[fourierTr]]*/
  def fourierTrC(data: Array[Complex]): Array[Complex] = dvCToArray(breeze.signal.fourierTr( DenseVector(data) ))
  /**See [[fourierTr]]*/
  def iFourierTrC(data: Array[Complex]): Array[Complex] = dvCToArray(breeze.signal.iFourierTr( DenseVector(data) ))
  /**See [[fourierTr]]*/
  def fourierTr2C(data: Array[Array[Complex]]): Array[Array[Complex]] = breeze.signal.fourierTr( DenseMatrix(data) ).toArray

//  /**Shift the zero-frequency component to the center of the spectrum.
//    * Use fourierShiftC instead for complex array input.
//    * This function swaps half-spaces for all axes listed (defaults to all). Note that y[0] is the Nyquist component only if len(x) is even.
//    *
//    * @param data input array
//    * @return
//    */
//  def fourierShift(data: Array[Double]): Array[Double] = breeze.signal.fourierShift( DenseVector(data) ).toArray
//  /**See [[fourierShift]]*/
//  def fourierShiftC(data: Array[Complex]): Array[Complex] = dvCToArray( breeze.signal.fourierShift( DenseVector(data) ) )
//  /**Shift the zero-frequency component to the center of the spectrum.
//    * Use fourierShiftC instead for complex array input.
//    * This function swaps half-spaces for all axes listed (defaults to all). Note that y[0] is the Nyquist component only if len(x) is even.
//    *
//    * @param data input array
//    * @return
//    */
//  def iFourierShift(data: Array[Double]): Array[Double] = breeze.signal.iFourierShift( DenseVector(data) ).toArray
//  /**See [[fourierShift]]*/
//  def iFourierShiftC(data: Array[Complex]): Array[Complex] = breeze.signal.iFourierShift( DenseVector(data) ).toArray
//
//  /**Returns the frequencies for each tap in a discrete Fourier transform, useful for plotting.
//    * You must specify either an fs or a dt argument. If you specify both, which is redundant,
//    * fs == 1.0/dt must be true.
//    *
//    * f = [0, 1, ..., n/2-1, -n/2, ..., -1] / (dt*n)         if n is even
//    * f = [0, 1, ..., (n-1)/2, -(n-1)/2, ..., -1] / (dt*n)   if n is odd
//    *
//    * @param windowLength window length of discrete Fourier transform
//    * @param fs  sampling frequency (Hz)
//    * @param shifted whether to return fourierShift'ed frequencies, default=false
//    */
//  def fourierFreq(windowLength: Int, fs: Double, shifted: Boolean): Array[Double] = breeze.signal.fourierFreq( windowLength, fs, -1, shifted).toArray
//  /**Returns the frequencies for each tap in a discrete Fourier transform, useful for plotting.
//    * You must specify either an fs or a dt argument. If you specify both, which is redundant,
//    * fs == 1.0/dt must be true.
//    *
//    * f = [0, 1, ..., n/2-1, -n/2, ..., -1] / (dt*n)         if n is even
//    * f = [0, 1, ..., (n-1)/2, -(n-1)/2, ..., -1] / (dt*n)   if n is odd
//    *
//    * @param windowLength window length of discrete Fourier transform
//    * @param fs  sampling frequency (Hz)
//    */
//  def fourierFreq(windowLength: Int, fs: Double): Array[Double] = breeze.signal.fourierFreq( windowLength ).toArray
//
//  /** Median filter the input data. Edge values are median-filtered with shorter windows, in order to preserve
//    * the total length of the input.
//    * @param windowLength only supports odd windowLength values, since even values would cause half-frame time shifts in one or the other direction,
//    *                     and would also lead to floating point values even for integer input
//    */
//  def filterMedian(data: Array[Double], windowLength: Int) = breeze.signal.filterMedian(DenseVector(data), windowLength).toArray
//



}
