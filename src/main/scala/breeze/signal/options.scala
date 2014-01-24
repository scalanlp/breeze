package breeze.signal

import breeze.linalg.DenseVector

/**Specifies all possible option objects for the breeze.signal package
  *
 * @author ktakagaki
 */

/**Base class for all options*/
abstract class Opt

///**Generic option for default*/
//case class OptDefault() extends Opt
////these implicit conversions will allow the same OptDefault() object to be given for different functions.
//object OptDefault {
////  implicit def optDefaultSpecialize_ConvolveOverhang(x: breeze.signal.OptDefault) = breeze.signal.OptConvolveOverhang.OptDefault
////  implicit def optDefaultSpecialize_Padding(x: breeze.signal.OptDefault) = breeze.signal.OptPadding.OptDefault
////  implicit def optDefaultSpecialize_ConvolveMethod(x: breeze.signal.OptDefault) = breeze.signal.OptConvolveMethod.OptDefault
////  implicit def optDefaultSpecialize_KernelType(x: breeze.signal.OptDefault) = breeze.signal.OptKernelType.OptDefault
////  implicit def optDefaultSpecialize_WindowFunction(x: breeze.signal.OptDefault) = breeze.signal.OptWindowFunction.OptDefault
//}

/**Generic option for default*/
case class OptNone() extends Opt
//these implicit conversions will allow the same OptNone() object to be given for different functions.
object OptNone {
  implicit def optNoneSpecialize_WindowFunction(x: breeze.signal.OptNone) = OptWindowFunction.OptNone()
  //implicit def optNoneSpecialize_Padding(x: breeze.signal.OptNone) = breeze.signal.OptPadding.OptNone
}

/**Generic option for automatic*/
case class OptAutomatic() extends Opt
//these implicit conversions will allow the same OptNone() object to be given for different functions.
object OptAutomatic {
  implicit def optNoneSpecialize_ConvolveMethod(x: breeze.signal.OptAutomatic) = OptConvolveMethod.OptAutomatic()
}



/**Option values: window function for filter design.*/
abstract class OptWindowFunction extends Opt
object OptWindowFunction {
  case class OptHamming(alpha: Double = 0.54, beta: Double = 0.46) extends OptWindowFunction {
    override def toString = "Hamming window ("+ alpha + ", " + beta + ")"
  }
//  case class OptDefault() extends OptHamming()
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
//  case class OptDefault() extends OptConvolveOverhang.OptSequence(-1, 1)
  /**Forms the cyclic convolution whose first element contains data(0)*kernel(k0),
    * and the last element contains data(-1)*kernel(k1).
    *
    * Common settings are:
    * {-1, 1}: no overhangs (Default)
    * {-1, -1}: maximal overhang at right hand end
    * {1, 1}: maximal overhang at left hand end
    * {1, -1}: maximal overhangs at both ends.
    */
  case class OptSequence(k0: Int, k1: Int) extends OptConvolveOverhang
  /**Forms the cyclic convolution where the kth kernel element is aligned with each data element.*/
  case class OptInteger(k: Int) extends OptConvolveOverhang
}

/**Option values: how to deal with convolution and filter padding.*/
abstract class OptPadding
object OptPadding{
  /**Option value: Performs cyclical convolutions (ie no padding)*/
  case class OptCyclical() extends OptPadding
//  /**Option value: Default padding is cyclical.*/
//  case class OptDefault() extends OptNone
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
//  /**Option value: Default option is OptAutomatic.*/
//  case class OptDefault() extends OptAutomatic
  /**Option value: Convolve using FFT.*/
  case class OptFFT() extends OptConvolveMethod
  /**Option value: Convolve using for loop.*/
  case class OptLoop() extends OptConvolveMethod
}

abstract class OptKernelType
object OptKernelType{
  /**Option value: use firwin() to design FIR kernel using window method.*/
  case class OptFirwin() extends OptKernelType
//  /**Option value: Default option is OptFirWin.*/
//  case class OptDefault() extends OptFirwin
}