package breeze.signal

import breeze.linalg.DenseVector

/**Specifies all possible option objects for the breeze.signal package
  *
 * @author ktakagaki
 */


///General options-related


/**Base class for all options*/
abstract class Opt


/**Generic option for none.*/
case class OptNone() extends Opt
object OptNone {
  //these implicit conversions will allow breeze.signal.OptNone() object to be given for different functions.
  implicit def optNoneSpecialize_WindowFunction(x: OptNone) = OptWindowFunction.OptNone()
  //implicit def optNoneSpecialize_Padding(x: breeze.signal.OptNone) = breeze.signal.OptPadding.OptNone()
}

/**Generic option for automatic.*/
case class OptAutomatic() extends Opt
object OptAutomatic {
  //these implicit conversions will allow breeze.signal.OptNone() object to be given for different functions.
  implicit def optNoneSpecialize_ConvolveMethod(x: OptAutomatic) = OptConvolveMethod.OptAutomatic()
}


///Individual Options

/**Option values: window function for filter design.*/
abstract class OptWindowFunction extends Opt
object OptWindowFunction {
  case class OptHamming(alpha: Double = 0.54, beta: Double = 0.46) extends OptWindowFunction {
    override def toString = "Hamming window ("+ alpha + ", " + beta + ")"
  }
  case class OptDenseVector(dv: DenseVector[Double]) extends OptWindowFunction {
    override def toString = "user-specified window"
  }
  case class OptNone() extends OptWindowFunction{
    override def toString = "no window"
  }
}


/**Option values: how to deal with convolution overhangs.*/
abstract class OptConvolveOverhang extends Opt
object OptConvolveOverhang{
  /**Option value: Forms the cyclic convolution whose first element contains data(0)*kernel(k0),
    * and the last element contains data(-1)*kernel(k1).
    *
    * Common settings are:
    * {-1, 1}: no overhangs (Default)
    * {-1, -1}: maximal overhang at right hand end
    * {1, 1}: maximal overhang at left hand end
    * {1, -1}: maximal overhangs at both ends.
    */
  case class OptSequence(k0: Int, k1: Int) extends OptConvolveOverhang
  /**Option value: Forms the cyclic convolution where the kth kernel element is aligned with each data element.*/
  case class OptInteger(k: Int) extends OptConvolveOverhang
}

/**Option values: how to deal with convolution and filter padding.*/
abstract class OptPadding
object OptPadding{
  /**Option value: Performs cyclical convolutions (ie no padding)*/
  case class OptCyclical() extends OptPadding
  /**Option value: Pads with the first and last components of the data*/
  case class OptBoundary() extends OptPadding
  /**Option value: Pads with a specific value, eg 0.*/
  case class OptValue[T](value: T) extends OptPadding
}

/**Option values: how to deal with convolution and filter padding.*/
abstract class OptConvolveMethod
object OptConvolveMethod{
  /**Option value: Decides on the fastest convolve method based on data size and type.*/
  case class OptAutomatic() extends OptConvolveMethod
  /**Option value: Convolve using FFT.*/
  case class OptFFT() extends OptConvolveMethod
  /**Option value: Convolve using for loop.*/
  case class OptLoop() extends OptConvolveMethod
}

abstract class OptKernelType
object OptKernelType{
  /**Option value: use firwin() to design FIR kernel using window method.*/
  case class OptFirwin() extends OptKernelType
}