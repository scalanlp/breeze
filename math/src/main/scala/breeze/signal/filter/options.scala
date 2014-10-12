package breeze.signal.filter

import breeze.linalg.DenseVector
import breeze.util.Opt


  @deprecated("filter design functions are distributed to different objects, eg FilterButterworth", "0.11")
  abstract class OptDesignMethod extends Opt
  @deprecated("filter design functions are distributed to different objects, eg FilterButterworth", "0.11")
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

  /**Option values: window function for filter design.*/
  abstract class OptWindowFunction extends Opt
  object OptWindowFunction {
    case class Hamming(alpha: Double = 0.54, beta: Double = 0.46) extends OptWindowFunction {
      override def toString = "Hamming window ("+ alpha + ", " + beta + ")"
    }
    case class Hanning(alpha: Double = 0.5, beta: Double = 0.5) extends OptWindowFunction {
      override def toString = "Hanning window ("+ alpha + "," + beta + ")"
    }
    case class Blackman(a0: Double = 0.42, a1: Double  = 0.5, a2: Double = 0.08) extends OptWindowFunction {
      override def toString = "Blackman window ("+ a0 + a1 + a2 + ")"
    }
    case class User(dv: DenseVector[Double]) extends OptWindowFunction {
      override def toString = "user-specified window"
    }
    case object None extends OptWindowFunction{
      override def toString = "no window"
    }
  }

  // filter type options of IIR/ SOS filter designs
  abstract class OptFilterTpe extends Opt

  object OptFilterTpe{
    case object LowPass extends OptFilterTpe
    case object HighPass extends OptFilterTpe
    case object BandPass extends OptFilterTpe
    case object BandStop extends OptFilterTpe
  }

  // filter order options
  abstract class OptOrder extends Opt

  object OptOrder {
    case object Automatic extends OptOrder
    case class IntValue(n: Int) extends OptOrder
  }

  // filter cutoff freq options
  abstract class OptOmega extends Opt

  object OptOmega{
    case class DoubleValue(omega: Double) extends OptOmega
    case class TupleValue(omega1: Double, omega2: Double) extends OptOmega
  }
