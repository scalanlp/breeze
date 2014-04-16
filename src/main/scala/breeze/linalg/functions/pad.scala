//package breeze.linalg
//
//import breeze.generic.UFunc
//import breeze.macros.expand
//import breeze.util.Opt
//import breeze.stats.{mean, median}
//import breeze.linalg.OptPadDimensions.Dim1
//import breeze.linalg.OptPadMode.Value
//import breeze.linalg
//
//
//// <editor-fold defaultstate="collapsed" desc=" OptPadMode ">
//
//sealed abstract class OptPadMode extends Opt
//object OptPadMode {
//  case object Zero    extends OptPadMode
//  case object Max     extends OptPadMode
//  case object Min     extends OptPadMode
//  case object Mean    extends OptPadMode
//  case object Median  extends OptPadMode
//
//  case class Value[T](n: T) extends OptPadMode
////  case class ValueInt(n: Int) extends OptPadMode
////  case class ValueLong(n: Long) extends OptPadMode
////  case class ValueFloat(n: Float) extends OptPadMode
////  case class ValueDouble(n: Double) extends OptPadMode
//
//  case object Wrap    extends OptPadMode
//  case object Reflect extends OptPadMode
//}
//
//// </editor-fold>
//
//// <editor-fold defaultstate="collapsed" desc=" OptPadDimensions ">
//
//sealed abstract class OptPadDimensions extends Opt
//object OptPadDimensions {
//
//  //This option class specifies a 1 dimensional padding result
//  case class Dim1(n1: Int) extends OptPadDimensions
//  //This option class specifies a 2 dimensional padding result
//  case class Dim2(n1: Int, n2: Int) extends OptPadDimensions
//
//}
//
//// </editor-fold>
//
//
//
///**
// * Pads a DenseVectors/DenseMatrix on the right side.
// */
//object padRight extends UFunc {
//
//  // <editor-fold defaultstate="collapsed" desc=" pad DenseVector in 1D ">
//
//  //pad with 1D dimension specification (OptPadDimensions.T1)
//  @expand
//  @expand.valify
//  implicit def implDV_OptPadDim_OptPadMode[@expand.args(/*Int, Long, Float,*/ Double) T]/*(
//    implicit /* @expand.sequence[T]( (p: Double) => p.toInt, (p: Double) => p.toLong, (p: Double) => p.toFloat, (p: Double) => p) conv: Double => T,*/
//    ct: ClassTag[T]) */:
//    Impl3[DenseVector[T], Dim1, OptPadMode, DenseVector[T]] = {
//
//    new Impl3[DenseVector[T], Dim1, OptPadMode, DenseVector[T] ] {
//
//      def apply(v: DenseVector[T], optDim: Dim1, optMode: OptPadMode ): DenseVector[T] = {
//
//        optMode match {
////          case OptPadMode.Zero     => padRight1Fixed( v, optDim, v(0) * 0 )
////          case OptPadMode.Max      => padRight1Fixed( v, optDim, max(v) )
////          case OptPadMode.Min      => padRight1Fixed( v, optDim, min(v) )
////          case OptPadMode.Mean     => padRight1Fixed( v, optDim, mean(v)/*conv( mean(convert(v, Double)))*/  )      //option "Mean" with Int will return approximate Int mean for padding....
////          case OptPadMode.Median   => padRight1Fixed( v, optDim, median(v)/*conv(median(v))*/ )    //option "Median" with Int will return approximate Int median for padding....
//          case OptPadMode.Value(n: T)    => padRight1Fixed( v, optDim, n )
////
////          case OptPadMode.Wrap     => padRight1DV( v, optDim, v )
////          case OptPadMode.Reflect  => padRight1DV( v, optDim, reverse(v) )
//
//          case _                   => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
//        }
//      }
//    }
//  }
//
//
//  //specify dimensions with Int
//  @expand
//  @expand.valify
//  implicit def implDV_Int_OptPadMode[@expand.args(/*Int, Long, Float,*/ Double) T]: Impl3[DenseVector[T], Int,  OptPadMode, DenseVector[T]] = {
//    new Impl3[DenseVector[T], Int, OptPadMode, DenseVector[T]] {
//      def apply(v: DenseVector[T], n: Int, optPadMode: OptPadMode): DenseVector[T] = padRight(v, Dim1(n), optPadMode)
//    }
//  }
//
//
//  //pad with zero (default)
//  @expand
//  @expand.valify
//  implicit def implDV_0[@expand.args(/*Int, Long, Float,*/ Double) T]: Impl2[DenseVector[T], Tuple1[Int], DenseVector[T]] = {
//    new Impl2[DenseVector[T], Tuple1[Int], DenseVector[T]] {
//      def apply(v: DenseVector[T], n: Tuple1[Int]): DenseVector[T] = padRight(v, Dim1(n._1), Value( v(0) ) )
//    }
//  }
//
//  //pad with dimension specification and zero (default)
//  @expand
//  @expand.valify
//  implicit def implDV_Int[@expand.args(/*Int, Long, Float, */Double) T]: Impl2[DenseVector[T], Int, DenseVector[T]] = {
//  new Impl2[DenseVector[T], Int, DenseVector[T]] {
//    def apply(v: DenseVector[T], n: Int): DenseVector[T] = padRight(v, (n))
//  }}
//
//  // </editor-fold>
//
//}
//
//// <editor-fold defaultstate="collapsed" desc=" 1D implementation classes ">
//
////pad 1D with fixed value
//object padRight1Fixed extends UFunc {
//
//  @expand
//  @expand.valify
//  implicit def padRightImpl1Fixed[@expand.args(Int, Long, Float, Double) T]: Impl3[DenseVector[T], Dim1, T, DenseVector[T]] = {
//    new Impl3[DenseVector[T], Dim1, T, DenseVector[T] ] {
//      def apply(v: DenseVector[T], optDim: Dim1, padValue: T): DenseVector[T] = {
//        require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
//        v.length match {
//          case optDim.n1 => v.copy
//          case num: Int if num < optDim.n1 => DenseVector.vertcat( v, DenseVector.tabulate(optDim.n1 - num)(p => padValue) )
//          case num: Int if optDim.n1 < num => v(0 until optDim.n1).copy //function should return a copy
//          case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
//        }
//      }
//    }
//  }
//
//}
//
////pad 1D with specific DenseVector values
//object padRight1DV extends UFunc {
//
//  @expand
//  @expand.valify
//  implicit def padRightImpl1DV[@expand.args(Int, Long, Float, Double) T]: Impl3[DenseVector[T], Dim1, DenseVector[T], DenseVector[T]] = {
//    new Impl3[DenseVector[T], Dim1, DenseVector[T], DenseVector[T] ] {
//      def apply(v: DenseVector[T], optDim: Dim1, padDV: DenseVector[T]): DenseVector[T] = {
//        require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
//        require( optDim.n1 - v.length <= padDV.length, "Cannot pad beyond specified padding DenseVector!")
//        v.length match {
//          case optDim.n1 => v.copy
//          case num: Int if num < optDim.n1 => DenseVector.vertcat( v, padDV(0 until optDim.n1 - num) )
//          case num: Int if optDim.n1 < num => v(0 until optDim.n1).copy //function should return a copy
//          case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
//        }
//      }
//    }
//  }
//
//}
//
//// </editor-fold>
//
//
