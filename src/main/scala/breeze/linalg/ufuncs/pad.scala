package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.util.Opt
import scala.reflect.ClassTag
import breeze.stats.{mean, median}

// <editor-fold defaultstate="collapsed" desc=" OptPadMode ">

abstract class OptPadMode extends Opt
object OptPadMode {
  case object Zero    extends OptPadMode
  case object Max     extends OptPadMode
  case object Min     extends OptPadMode
  case object Mean    extends OptPadMode
  case object Median  extends OptPadMode

  case class Value[T](n: T) extends OptPadMode
  implicit def intToOptPadMode[T](n: T)(implicit ct: ClassTag[T]): OptPadMode.Value[T] = Value( n )

  case object Wrap    extends OptPadMode
  case object Reflect extends OptPadMode

}

// </editor-fold>

// <editor-fold defaultstate="collapsed" desc=" OptPadDimensions ">

abstract class OptPadDimensions extends Opt
object OptPadDimensions {

  //This option class specifies a 1 dimensional padding result
  case class T1(n: Tuple1[Int]) extends OptPadDimensions
  //option can be entered as Int or Tuple1 as well
  implicit def intToOptPadDimensions_T1(n: Int): OptPadDimensions.T1 = T1( Tuple1( n ) )
  implicit def tuple1ToOptPadDimensions_T1(n: Tuple1[Int]): OptPadDimensions.T1 = T1( n )

  //This option class specifies a 2 dimensional padding result
  case class T2(n: Tuple2[Int, Int]) extends OptPadDimensions
  //option can be entered as Tuple2 as well
  implicit def tuple2ToOptPadDimensions_T2(n: Tuple2[Int, Int]): OptPadDimensions.T2 = T2( n )

}

// </editor-fold>



/**
 * Pads a DenseVectors/DenseMatrix on the right side.
 */
object padRight extends UFunc {

  // <editor-fold defaultstate="collapsed" desc=" pad DenseVector in 1 dimension ">

  //pad with 1D dimension specification (OptPadDimensions.T1)
  @expand
  @expand.valify
  implicit def implDV1[@expand.args(Int, Long, Float, Double) T: ClassTag]: Impl3[DenseVector[T], OptPadDimensions.T1, OptPadMode, DenseVector[T]] = {
    new Impl3[DenseVector[T], OptPadDimensions.T1, OptPadMode, DenseVector[T] ] {
      def apply(v: DenseVector[T], optDim: OptPadDimensions.T1, optMode: OptPadMode = OptPadMode.Zero ): DenseVector[T] = {
        optMode match {
          case OptPadMode.Zero     => padRightImpl1Fixed( v, optDim, v(0) * 0 )
          case OptPadMode.Max      => padRightImpl1Fixed( v, optDim, max(v) )
          case OptPadMode.Min      => padRightImpl1Fixed( v, optDim, min(v) )
          case OptPadMode.Mean     => padRightImpl1Fixed( v, optDim, convert(mean(v), ClassTag[T]) )      //option "Mean" with Int will return approximate Int mean for padding....
          case OptPadMode.Median   => padRightImpl1Fixed( v, optDim, convert(median(v), ClassTag[T]) )    //option "Median" with Int will return approximate Int median for padding....
          case value: OptPadMode.Value[T]    => padRightImpl1Fixed( v, optDim, value.n )

          case OptPadMode.Wrap     => padRightImpl1DV( v, optDim, v )
          case OptPadMode.Reflect  => padRightImpl1DV( v, optDim, reverse(v) )

          case _                   => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
        }
      }
    }
  }

  //pad 1D with fixed value
  @expand
  @expand.valify
  def padRightImpl1Fixed[@expand.args(Int, Long, Float, Double) T]: Impl3[DenseVector[T], OptPadDimensions.T1, T, DenseVector[T]] = {
    new Impl3[DenseVector[T], OptPadDimensions.T1, T, DenseVector[T] ] {
      def apply(v: DenseVector[T], optDim: OptPadDimensions.T1, padValue: T): DenseVector[T] = {
        require( optDim.n._1 > 0, "Cannot pad to zero or negative length!")
        v.length match {
          case optDim.n._1 => v.copy
          case num: Int if num < optDim.n._1 => DenseVector.vertcat( v, DenseVector.tabulate(optDim.n._1 - num)(p => padValue) )
          case num: Int if optDim.n._1 < num => v(0 until optDim.n._1).copy //function should return a copy
          case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.n.toString + " !")
        }
      }
    }
  }

  //pad 1D with specific DenseVector values
  @expand
  @expand.valify
  def padRightImpl1DV[@expand.args(Int, Long, Float, Double) T]: Impl3[DenseVector[T], OptPadDimensions.T1, DenseVector[T], DenseVector[T]] = {
    new Impl3[DenseVector[T], OptPadDimensions.T1, DenseVector[T], DenseVector[T] ] {
      def apply(v: DenseVector[T], optDim: OptPadDimensions.T1, padDV: DenseVector[T]): DenseVector[T] = {
        require( optDim.n._1 > 0, "Cannot pad to zero or negative length!")
        require( optDim.n._1 - v.length <= padDV.length, "Cannot pad beyond specified padding DenseVector!")
        v.length match {
          case optDim.n._1 => v.copy
          case num: Int if num < optDim.n._1 => DenseVector.vertcat( v, padDV(0 until optDim.n._1 - num) )
          case num: Int if optDim.n._1 < num => v(0 until optDim.n._1).copy //function should return a copy
          case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.n.toString + " !")
        }
      }
    }
  }


//  //pad with zero (default)
//  @expand
//  @expand.valify
//  implicit def implDV_Int_0[@expand.args(Int, Long, Float, Double) T]: Impl2[DenseVector[T], Tuple1[Int], DenseVector[T]] = {
//    new Impl3[DenseVector[T], Tuple1[Int], DenseVector[T]] {
//      def apply(v: DenseVector[T], n: Tuple1[Int], padValue: T): DenseVector[T] = padRight(v, n, v.zero)
//    }
//  }
//
//  //pad with dimension specification and zero (default)
//  @expand
//  @expand.valify
//  implicit def implDV_Int[@expand.args(Int, Long, Float, Double) T]: Impl2[DenseVector[T], Int, DenseVector[T]] = {
//  new Impl2[DenseVector[T], Int, DenseVector[T]] {
//    def apply(v: DenseVector[T], n: Int): DenseVector[T] = padRight(v, (n))
//  }}







}
