package breeze.linalg

import breeze.util.Opt

/**
 * @author ktakagaki
 * @date 04/17/2014.
 */
object Options {

  //Options for CanPad

  // <editor-fold defaultstate="collapsed" desc=" OptPadDimensions ">

  sealed abstract class OptPadDimensions extends Opt
  //This option class specifies a 1 dimensional padding result
  case class Dimensions1(n1: Int) extends OptPadDimensions
  //This option class specifies a 2 dimensional padding result
  case class Dimensions2(n1: Int, n2: Int) extends OptPadDimensions

  implicit def intToDimensions1(n: Int): Dimensions1 = Dimensions1(n)
  implicit def t1ToDimensions1(n: Tuple1[Int]): Dimensions1 = Dimensions1(n._1)
  implicit def t2ToDimensions2(n: Tuple2[Int, Int]): Dimensions2 = Dimensions2(n._1, n._2)

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" OptPadMode ">

  sealed abstract class OptPadMode extends Opt
  case object Zero    extends OptPadMode
  case object Max     extends OptPadMode
  case object Min     extends OptPadMode
  case object Mean    extends OptPadMode
  case object Median  extends OptPadMode
  case class Value[T](n: T) extends OptPadMode
  //  case class ValueInt(n: Int) extends OptPadMode
  //  case class ValueLong(n: Long) extends OptPadMode
  //  case class ValueFloat(n: Float) extends OptPadMode
  //  case class ValueDouble(n: Double) extends OptPadMode
  case object Wrap    extends OptPadMode
  case object Reflect extends OptPadMode

  implicit def tToOptModeValue[T](n: T): OptPadMode = Value(n)

  // </editor-fold>

}
