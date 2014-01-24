package breeze.signal.filter

import breeze.linalg.{sum, DenseVector, diff}
import breeze.numerics.{cos, sincpi, isOdd, isEven}
import breeze.signal._
import scala.math.{sin, Pi}

/**
 * Portions of the code are translated from scipy (scipy.org) based on provisions of the BSD license.
 *
 * @author ktakagaki
 */
abstract class FilterKernel {
  val designText: String
  override def toString = this.getClass.getSimpleName + "(): " + designText
}

abstract class FilterKernel1D extends FilterKernel

case class FIRKernel1D(val kernel: DenseVector[Double], override val designText: String) extends FilterKernel1D {
  lazy val kernelFourier = fourierTransform(kernel)
  lazy val length = kernel.length
}

case class IIRKernel1D(kernelA: DenseVector[Double], kernelB: DenseVector[Double], override val designText: String) extends FilterKernel {
}

object KernelDesign{

  /** FIR filter design using the window method.

    This function computes the coefficients of a finite impulse response
    filter.  The filter will have linear phase; it will be Type I if
  `numtaps` is odd and Type II if `numtaps` is even.

    Type II filters always have zero response at the Nyquist rate, so a
    ValueError exception is raised if firwin is called with `numtaps` even and
  having a passband whose right end is at the Nyquist rate.

    @param cutoff Cutoff frequencies of the filter, specified in units of "nyquist."
                  The frequencies should all be positive and monotonically increasing.
                  The frequencies must lie between (0, nyquist).
                  0 and nyquist should not be included in this array.

    @param optWindow Currently supports a hamming window [[breeze.signal.OptWindowFunction.OptHamming]],
                  a specified window [[breeze.signal.OptWindowFunction.OptDenseVector]], or
                  no window [[breeze.signal.OptWindowFunction.OptNone]].
    @param zeroPass If true (default), the gain at frequency 0 (ie the "DC gain") is 1, if false, 0.
    @param scale Whether to scale the coefficiency so that frequency response is unity at either (A) 0 if zeroPass is true
                 or (B) at nyquist if the first passband ends at nyquist, or (C) the center of the first passband. Default is true.
    @param nyquist The nyquist frequency, default is 1.
    */
  def firwin(numtaps: Int, cutoff: DenseVector[Double], optWindow: OptWindowFunction = OptWindowFunction.OptHamming(),
              zeroPass: Boolean = true, nyquist: Double = 1d, scale: Boolean = true): FIRKernel1D = {

    //various variable conditions which must be met
    require(cutoff.length > 0, "At least one cutoff frequency must be given!")
    require(cutoff.min >= 0, "The cutoff frequencies must be bigger than zero!")
    require(cutoff.max <= nyquist, "The cutoff frequencies must be smaller than the nyquist frequency!")
    if(cutoff.length > 1){
      require(diff(cutoff).min > 0, "The cutoff frequency must be monotonically increasing.")
    }

    val nyquistPass = zeroPass != isOdd(cutoff.length)
    var tempCutoff = (cutoff / nyquist).toArray
    if(zeroPass) tempCutoff = tempCutoff.+:(0d)
    if(nyquistPass) tempCutoff = tempCutoff.:+(1d)
    val scaledCutoff = DenseVector(tempCutoff)


    //ToDo: Is the following statement translated from numpy code correct???
    //https://github.com/scipy/scipy/blob/v0.13.0/scipy/signal/fir_filter_design.py#L138
    require( !(nyquistPass && isEven(cutoff.length) ),
      "A filter with an even number of coefficients must have zero response at the Nyquist rate.")

    //val bands = scaledCutoff.reshape(-1, 2)
    val alpha = 0.5 * (numtaps -1)
    val m = DenseVector.tabulate(numtaps)( i => i.toDouble ) - alpha


    val h = DenseVector.zeros[Double]( m.length )
    for(band <- scaledCutoff.toArray.zipWithIndex ) {
      if( band._2 % 2 == 0 ) h -= sincpi(m :* band._1) :* band._1
      else h += sincpi(m :* band._1) :* band._1
    }

    val win = optWindow match {
      case OptWindowFunction.OptHamming(alpha, beta) => WindowFunctions.hammingWindow( numtaps, alpha, beta )
      case OptWindowFunction.OptNone() => DenseVector.ones[Double]( numtaps )
      case OptWindowFunction.OptDenseVector(dv) => {
        require(dv.length == numtaps, "Length of specified window function is not the same as numtaps!")
        dv
      }
    }

    h *= win

    if(scale){
      val scaleFrequency =
        if(scaledCutoff(0) == 0d) 0d
        else if(scaledCutoff(1) == 1d) 1d
        else (scaledCutoff(0) + scaledCutoff(1))/2d
      val c = cos( m :* (Pi * scaleFrequency) )
      val s = sum( h :* c )
      h /= s
    }

    new FIRKernel1D( h,
      "FIRKernel1D(firwin): " + numtaps + " taps, " + cutoff + ", " + optWindow + ", zeroPass=" + zeroPass + ", nyquist=" + nyquist + ", scale=" + scale
    )

  }

}