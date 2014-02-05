package breeze.signal

import breeze.linalg.DenseVector

/**Specifies all possible option objects for the breeze.signal package
  *
 * @author ktakagaki
 */


///General options-related


/**Base class for all options*/
abstract class Opt

//Generic options with implicit specialization cannot be employed for case objects
// (they can for parameterless case classes)
//
///**Generic option for none.*/
//case object None extends Opt {
//  //these implicit conversions will allow breeze.signal.None() object to be given for different functions.
//  implicit def optNoneSpecialize_WindowFunction(x: breeze.signal.None) = OptWindowFunction.None
//  implicit def optNoneSpecialize_ConvolveOverhang(x: breeze.signal.None) = OptOverhang.None
//  //implicit def optNoneSpecialize_Padding(x: breeze.signal.None) = breeze.signal.OptPadding.None()
//}
//
///**Generic option for automatic.*/
//case object Automatic extends Opt {
//  //these implicit conversions will allow breeze.signal.None() object to be given for different functions.
//  implicit def optNoneSpecialize_ConvolveMethod(x: breeze.signal.Automatic) = OptMethod.Automatic
//}


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
  case object None extends OptWindowFunction{
    override def toString = "no window"
  }
}


/**Option values: how to deal with convolution overhangs.*/
abstract class OptOverhang extends Opt
object OptOverhang{
  /** THIS OPTION REMOVED FOR NOW, DUE TO AMBIGUITY WITH KERNEL DIRECTION (IE CONVOLVE VS CORRELATE)
   * Option value: Forms the cyclic convolution whose first element contains data(0)*kernel(k0),
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
  case object None extends OptOverhang
  /**Option value: maximal overhangs, equivalent to MatLab conv default ('full'), equivalent to Sequence(1, -1).*/
  case object Full extends OptOverhang
  /**Option value: maximal overhangs, equivalent to MatLab conv default ('full'), equivalent to Sequence(1, -1).*/
  case object PreserveLength extends OptOverhang
//  /**Option value: Forms the cyclic convolution where the kth kernel element is aligned with each data element.*/
//  case class OptInteger(k: Int) extends OptOverhang
}

/**Option values: how to deal with convolution and filter padding.*/
abstract class OptPadding extends Opt
object OptPadding{
  /**Option value: Performs cyclical convolutions (ie no padding)*/
  case object Cyclical extends OptPadding
  /**Option value: Pads with the first and last components of the data*/
  case object Boundary extends OptPadding
  /**Option value: Pads with a specific value, eg 0.*/
  case class ValueOpt[T](value: T) extends OptPadding
}

/**Option values: how to deal with convolution and filter padding.*/
abstract class OptMethod extends Opt
object OptMethod{
  /**Option value: Decides on the fastest convolve method based on data size and type.*/
  case object Automatic extends OptMethod
  /**Option value: Convolve using FFT.*/
  case object FFT extends OptMethod
  /**Option value: Convolve using for loop.*/
}

abstract class OptDesignMethod extends Opt
object OptDesignMethod {
  /**Option value: use firwin() to design FIR kernel using window method.*/
  case object Firwin extends OptDesignMethod
  case object Cheby1 extends OptDesignMethod
}

abstract class OptFilterOrder extends Opt
object OptFilterOrder {
  /**Option value: use firwin() to design FIR kernel using window method.*/
  case object Automatic extends OptFilterOrder
  case class IntOpt(n: Int) extends OptFilterOrder
}

abstract class OptRange extends Opt
object OptRange {
  case object All extends OptRange
  case class RangeOpt(r: Range) extends OptRange
  implicit def rangeToRangeOpt(r: Range) = RangeOpt( r )
}

