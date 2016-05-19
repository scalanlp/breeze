package breeze.signal.support

import breeze.signal.{OptWindowFunction}
import breeze.linalg._
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
  def apply(taps: Int, omegas: DenseVector[Double], nyquist: Double,
            zeroPass: Boolean, scale: Boolean, multiplier: Double,
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
      def apply(taps: Int, omegas: DenseVector[Double], nyquist: Double,
                zeroPass: Boolean, scale: Boolean, multiplier: Double,
                optWindow: OptWindowFunction  ): FIRKernel1D[Double]
      =  new FIRKernel1D[Double](
        firwinDoubleImpl(taps, omegas, nyquist, zeroPass,  scale, optWindow) * multiplier,
        multiplier,
        "FIRKernel1D(firwin): " + taps + " taps, " + omegas + ", " + optWindow + ", zeroPass=" + zeroPass + ", nyquist=" + nyquist + ", scale=" + scale
      )

    }
  }

  @expand
  implicit def firwinT[@expand.args(Int, Long, Float) T]: CanFirwin[T] = {
    new CanFirwin[T] {
      def apply(taps: Int, omegas: DenseVector[Double], nyquist: Double,
                zeroPass: Boolean, scale: Boolean, multiplier: Double,
                optWindow: OptWindowFunction  ): FIRKernel1D[T]
      =  new FIRKernel1D[T](
        convert( firwinDoubleImpl(taps, omegas, nyquist, zeroPass,  scale, optWindow) * multiplier, T),
        multiplier,
        "FIRKernel1D(firwin): " + taps + " taps, " + omegas + ", " + optWindow + ", zeroPass=" + zeroPass + ", nyquist=" + nyquist + ", scale=" + scale
      )

    }
  }

  def firwinDoubleImpl(taps: Int, omegas: DenseVector[Double], nyquist: Double,
            zeroPass: Boolean,  scale: Boolean,
            optWindow: OptWindowFunction  ): DenseVector[Double] = {
    //various variable conditions which must be met
    require(omegas.length > 0, "At least one cutoff frequency must be given!")
    require(min(omegas) >= 0, "The cutoff frequencies must be bigger than zero!")
    require(max(omegas) <= nyquist, "The cutoff frequencies must be smaller than the nyquist frequency!")
    if(omegas.length > 1){
      require(min(diff(omegas)) > 0, "The cutoff frequency must be monotonically increasing.")
    }

    val nyquistPass = ( zeroPass != isOdd(omegas.length) )
    var tempCutoff = (omegas / nyquist).toArray
    if(zeroPass) tempCutoff = tempCutoff.+:(0d)
    if(nyquistPass) tempCutoff = tempCutoff.:+(1d)
    val scaledCutoff = DenseVector(tempCutoff)


    //ToDo: Is the following statement translated from numpy code correctly???
    //https://github.com/scipy/scipy/blob/v0.13.0/scipy/signal/fir_filter_design.py#L138
    require( !(nyquistPass && isEven(taps) ),
      "A filter with an even number of taps must have zero response at the Nyquist rate.")

    //val bands = scaledCutoff.reshape(-1, 2)
    val alpha = 0.5 * (taps -1)
    val m = DenseVector.tabulate(taps)( i => i.toDouble ) - alpha

    val h = DenseVector.zeros[Double]( m.length )
    for(band <- scaledCutoff.toArray.zipWithIndex ) {
      if( isEven(band._2) ) h -= sincpi(m *:* band._1) *:* band._1
      else h += sincpi(m *:* band._1) *:* band._1
    }

    val win = optWindow match {
      case OptWindowFunction.Hamming(alpha, beta) => WindowFunctions.hammingWindow( taps, alpha, beta )
      case OptWindowFunction.Hanning(alpha, beta) => WindowFunctions.hammingWindow( taps, alpha, beta )
      case OptWindowFunction.Blackman(a0, a1, a2) => WindowFunctions.blackmanWindow( taps, a0, a1, a2)
      case OptWindowFunction.None => DenseVector.ones[Double]( taps )
      case OptWindowFunction.User(dv) => {
        require(dv.length == taps, "Length of specified window function is not the same as taps option!")
        dv
      }
    }

    h *= win

    if(scale){
      val scaleFrequency =
        if(scaledCutoff(0) == 0d) 0d
        else if(scaledCutoff(1) == 1d) 1d
        else (scaledCutoff(0) + scaledCutoff(1))/2d
      val c: DenseVector[Double] = cos( m *:* (Pi * scaleFrequency) )
      val s: Double = sum( h *:* c )
      h /= s
    }

    h
  }


}
