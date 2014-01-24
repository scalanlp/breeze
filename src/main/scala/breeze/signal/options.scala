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
case class None() extends Opt
object None {
  //these implicit conversions will allow breeze.signal.None() object to be given for different functions.
  implicit def optNoneSpecialize_WindowFunction(x: None) = OptWindowFunction.None()
  implicit def optNoneSpecialize_ConvolveOverhang(x: None) = OptOverhang.None()
  //implicit def optNoneSpecialize_Padding(x: breeze.signal.None) = breeze.signal.OptPadding.None()
}

/**Generic option for automatic.*/
case class Automatic() extends Opt
object Automatic {
  //these implicit conversions will allow breeze.signal.None() object to be given for different functions.
  implicit def optNoneSpecialize_ConvolveMethod(x: Automatic) = OptMethod.Automatic()
}


///Individual Options

/**Option values: window function for filter design.*/
abstract class OptWindowFunction extends Opt
object OptWindowFunction {
  case class Hamming(alpha: Double = 0.54, beta: Double = 0.46) extends OptWindowFunction {
    override def toString = "Hamming window ("+ alpha + ", " + beta + ")"
  }
  case class User(dv: DenseVector[Double]) extends OptWindowFunction {
    override def toString = "user-specified window"
  }
  case class None() extends OptWindowFunction{
    override def toString = "no window"
  }
}


/**Option values: how to deal with convolution overhangs.*/
abstract class OptOverhang extends Opt
object OptOverhang{
  /**Option value: Forms the cyclic convolution whose first element contains data(0)*kernel(k0),
    * and the last element contains data(-1)*kernel(k1).
    *
    * Common settings are:
    * {-1, 1}: no overhangs (Default)
    * {-1, -1}: maximal overhang at right hand end
    * {1, 1}: maximal overhang at left hand end
    * {1, -1}: maximal overhangs at both ends.
    */
  case class Sequence(k0: Int, k1: Int) extends OptOverhang
  /**Option value: Default, no overhangs, equivalent to Sequence(-1, 1).*/
  case class None() extends OptOverhang
  /**Option value: maximal overhangs, equivalent to MatLab conv default ('full'), equivalent to Sequence(1, -1).*/
  case class Full() extends OptOverhang
//  /**Option value: Forms the cyclic convolution where the kth kernel element is aligned with each data element.*/
//  case class OptInteger(k: Int) extends OptOverhang
}

/**Option values: how to deal with convolution and filter padding.*/
abstract class OptPadding extends Opt
object OptPadding{
  /**Option value: Performs cyclical convolutions (ie no padding)*/
  case class Cyclical() extends OptPadding
  /**Option value: Pads with the first and last components of the data*/
  case class Boundary() extends OptPadding
  /**Option value: Pads with a specific value, eg 0.*/
  case class Value[T](value: T) extends OptPadding
}

/**Option values: how to deal with convolution and filter padding.*/
abstract class OptMethod extends Opt
object OptMethod{
  /**Option value: Decides on the fastest convolve method based on data size and type.*/
  case class Automatic() extends OptMethod
  /**Option value: Convolve using FFT.*/
  case class FFT() extends OptMethod
  /**Option value: Convolve using for loop.*/
  case class Loop() extends OptMethod
}

abstract class OptKernelType extends Opt
object OptKernelType{
  /**Option value: use firwin() to design FIR kernel using window method.*/
  case class Firwin() extends OptKernelType
}