package breeze.signal.filters

import breeze.linalg.DenseVector
import breeze.signal.OptWindowFunction
import breeze.signal.support.{FIRKernel1D, CanFirwin}

/**
 * Created by Kenta on 08.10.2014.
 */
object FilterFirwin {

  def design[Output](taps: Int, omegas: DenseVector[Double], nyquist: Double = 1d,
                                 zeroPass: Boolean = true,
                                 scale: Boolean = true, multiplier: Double = 1d,
                                 optWindow: OptWindowFunction = OptWindowFunction.Hamming()  )
                                (implicit canFirwin: CanFirwin[Output]): FIRKernel1D[Output] =
    canFirwin(taps, omegas, nyquist, zeroPass, scale, multiplier, optWindow)

}
