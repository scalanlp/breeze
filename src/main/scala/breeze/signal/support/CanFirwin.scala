package breeze.signal.support

import breeze.signal.{OptWindowFunction}
import breeze.linalg.{diff, DenseVector, min, sum}
import breeze.numerics.{cos, isOdd, isEven, sincpi}
import scala.math.Pi
import breeze.macros.expand

/**
 * Construction delegate trait for convolving type InputType.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
 *
 * @author ktakagaki
 */
trait CanFirwin[Output] {
  def apply(numtaps: Int, cutoff: DenseVector[Double],
            zeroPass: Boolean, nyquist: Double, scale: Boolean,
            optWindow: OptWindowFunction  ): FIRKernel1D[Output]
}

/**
 * Construction delegate for firwin filter design.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use firwin(x: DenseVector).
 *
 * @author ktakagaki
 */
object CanFirwin {

  /** Use via implicit delegate syntax firwin(xxxx)
    *
    */
  implicit def firwinDouble: CanFirwin[Double] = {
    new CanFirwin[Double] {
      def apply(numtaps: Int, cutoff: DenseVector[Double],
                zeroPass: Boolean, nyquist: Double, scale: Boolean,
                optWindow: OptWindowFunction  ): FIRKernel1D[Double]
      =  new FIRKernel1D[Double](
        firwinDoubleImpl(numtaps, cutoff, zeroPass, nyquist, scale, optWindow),
        "FIRKernel1D(firwin): " + numtaps + " taps, " + cutoff + ", " + optWindow + ", zeroPass=" + zeroPass + ", nyquist=" + nyquist + ", scale=" + scale
      )

    }
  }

  def firwinDoubleImpl(numtaps: Int, cutoff: DenseVector[Double],
            zeroPass: Boolean, nyquist: Double, scale: Boolean,
            optWindow: OptWindowFunction  ): DenseVector[Double] = {

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
      case OptWindowFunction.Hamming(alpha, beta) => WindowFunctions.hammingWindow( numtaps, alpha, beta )
      case OptWindowFunction.None => DenseVector.ones[Double]( numtaps )
      case OptWindowFunction.User(dv) => {
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

    h
  }

  /** Use via implicit delegate syntax firwin(xxxx)
    *
    */
  @expand
  implicit def firwinT[@expand.args(Int, Long, Float) T]: CanFirwin[T] = {
    new CanFirwin[T] {
      def apply(numtaps: Int, cutoff: DenseVector[Double],
                zeroPass: Boolean, nyquist: Double, scale: Boolean,
                optWindow: OptWindowFunction  ): FIRKernel1D[T]
      =  new FIRKernel1D[T](
                (firwinDoubleImpl(numtaps, cutoff, zeroPass, nyquist, scale, optWindow)).map(_.asInstanceOf[T]),
                "FIRKernel1D(firwin): " + numtaps + " taps, " + cutoff + ", " + optWindow + ", zeroPass=" + zeroPass + ", nyquist=" + nyquist + ", scale=" + scale
            )
    }
  }

}
