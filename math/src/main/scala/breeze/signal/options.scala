package breeze.signal

import breeze.linalg.DenseVector
import breeze.util.Opt

/**Specifies all possible option objects for the breeze.signal package
  *
 * @author ktakagaki
 */


///General options-related

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
//  /** THIS OPTION REMOVED FOR NOW, DUE TO AMBIGUITY WITH KERNEL DIRECTION (IE CONVOLVE VS CORRELATE)
//   * Option value: Forms the cyclic convolution whose first element contains data(0)*kernel(k0),
//    * and the last element contains data(-1)*kernel(k1).
//    *
//    * Common settings are:
//    * {-1, 1}: no overhangs (Default)
//    * {-1, -1}: maximal overhang at right hand end
//    * {1, 1}: maximal overhang at left hand end
//    * {1, -1}: maximal overhangs at both ends.
//    */
//  case class Sequence(k0: Int, k1: Int) extends OptOverhang
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
  /**Option value: Pads with 0.*/
  case object Zero extends OptPadding
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
  case object IIR extends OptDesignMethod
  case object SOS extends OptDesignMethod
  case object Cheby1 extends OptDesignMethod
}

abstract class OptFilterTaps extends Opt
object OptFilterTaps {
  case object Automatic extends OptFilterTaps
  case class IntOpt(n: Int) extends OptFilterTaps
}

/**slices specific result ranges out of results for convolve, etc*/
abstract class OptRange extends Opt
object OptRange {
  case object All extends OptRange {
    override def toString() = "OptRange.All"
  }
  case class RangeOpt(r: Range) extends OptRange {
    override def toString() = "OptRange.RangeOpt( "+ r.start + ", "+ r.end+", "+r.step +"), isInclusive=" + r.isInclusive
  }
//  case class Single(i: Int) extends OptRange {
//    override def toString() = "OptRange.Single("+ i +")"
//  }
  implicit def rangeToRangeOpt(r: Range) = OptRange.RangeOpt( r )
}

// filter type options of IIR/ SOS filter designs
abstract class OptFilterType extends Opt

object OptFilterType{
  case object LowPass extends OptFilterType
  case object HighPass extends OptFilterType
  case object BandPass extends OptFilterType
  case object BandStop extends OptFilterType
}

// filter order options
abstract class OptOrder extends Opt

object OptOrder {
  case object Automatic extends OptOrder
  case class IntValue(n: Int) extends OptOrder
}

// filter cutoff freq options
abstract class OptOmega extends Opt

object OptOmega {
  case class DoubleValue(omega: Double) extends OptOmega
  case class TupleValue(omega1: Double, omega2: Double) extends OptOmega
}