package breeze.linalg

import breeze.macros.expand
import breeze.linalg.CanPadOpts._
import breeze.util.Opt

/**
 * @author ktakagaki
 * @date 04/16/2014.
 */
trait CanPadRight[Type, Dimensions] {
  def apply(v: DenseVector[Type], optDim: Dimensions, optMode: OptPadMode): DenseVector[Type]
}

object CanPadRight{

  // <editor-fold defaultstate="collapsed" desc=" DenseVector 1D padding ">

  @expand
  @expand.valify
  implicit def implDV_OptPadDim[@expand.args(/*Int, Long, Float, */Double) T]: CanPadRight[T, Dimensions1] =
    new CanPadRight[T, Dimensions1] {
        def apply(v: DenseVector[T], optDim: Dimensions1, optMode: OptPadMode): DenseVector[T] = {
          //println( v.toString + " " + optDim.toString + " " + optMode.toString + " " + (v(0) * 0).toString)
          optMode match {
            //case Zero     => padRight1ImplFixed( v, optDim, 0d )
            //case Max      => padRight1ImplFixed( v, optDim, max(v) )
//            case Min      => padRight1ImplFixed( v, optDim, min(v) )
//            //          case OptPadMode.Mean     => padRight1Fixed( v, optDim, mean(v)/*conv( mean(convert(v, Double)))*/  )      //option "Mean" with Int will return approximate Int mean for padding....
//            //          case OptPadMode.Median   => padRight1Fixed( v, optDim, median(v)/*conv(median(v))*/ )    //option "Median" with Int will return approximate Int median for padding....
//            case Value(n: T)    => padRight1ImplFixed( v, optDim, n )
//
//    //        case Wrap     => padRight1ImplDV( v, optDim, v )
//    //        case Reflect  => padRight1ImplDV( v, optDim, reverse(v) )

            case _                   => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
          }
        }
  }

  // </editor-fold>
  // <editor-fold defaultstate="collapsed" desc=" 1D Padding Implementation ">

  @expand
  def padRight1ImplFixed[@expand.args(Int, Long, Float, Double) T](v: DenseVector[T], optDim: Dimensions1, padValue: T): DenseVector[T] = {
      require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
      v.length match {
        case optDim.n1 => v.copy
        case num: Int if num < optDim.n1 => DenseVector.vertcat( v, DenseVector.tabulate(optDim.n1 - num)(p => padValue) )
        case num: Int if optDim.n1 < num => v(0 until optDim.n1).copy //function should return a copy
        case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
      }
  }

  @expand
  private def padRight1ImplDV[@expand.args(Int, Long, Float, Double) T](v: DenseVector[T], optDim: Dimensions1, padDV: DenseVector[T]): DenseVector[T] = {
      require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
      require( optDim.n1 - v.length <= padDV.length, "Cannot pad beyond specified padding DenseVector!")
      v.length match {
        case optDim.n1 => v.copy
        case num: Int if num < optDim.n1 => DenseVector.vertcat( v, padDV(0 until optDim.n1 - num) )
        case num: Int if optDim.n1 < num => v(0 until optDim.n1).copy //function should return a copy
        case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
      }
  }

  // </editor-fold>


//  // <editor-fold defaultstate="collapsed" desc=" DenseVector 2D padding ">
//
//  @expand
//  @expand.valify
//  implicit def implDM_OptPadDim_OptPadMode[@expand.args(/*Int, Long, Float,*/ Double) T]:
//  CanPadRight[DenseMatrix[T], OptPadDimensions, OptPadMode] ={
//
//    def apply(m: DenseMatrix[T], optDim: Dimensions2, optMode: OptPadMode): DenseMatrix[T] = {
//      optMode match {
//        case Zero     => padRight2ImplFixed( m, optDim, v(0) * 0 )
//        case Max      => padRight2ImplFixed( m, optDim, max(v) )
////        case Min      => padRight2ImplFixed( m, optDim, min(v) )
//        //          case OptPadMode.Mean     => padRight1Fixed( v, optDim, mean(v)/*conv( mean(convert(v, Double)))*/  )      //option "Mean" with Int will return approximate Int mean for padding....
//        //          case OptPadMode.Median   => padRight1Fixed( v, optDim, median(v)/*conv(median(v))*/ )    //option "Median" with Int will return approximate Int median for padding....
//        case Value(n: T)    => padRight1ImplFixed( m, optDim, n )
//        //
//        //          case OptPadMode.Wrap     => padRight1DV( v, optDim, v )
//        //          case OptPadMode.Reflect  => padRight1DV( v, optDim, reverse(v) )
//
//        case _                   => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
//      }
//    }
//  }
//
//  // </editor-fold>
//  // <editor-fold defaultstate="collapsed" desc=" 2D Padding Implementation ">
//
//  @expand
//  @expand.valify
//  private def padRight2ImplFixed[@expand.args(Int, Long, Float, Double) T](m: DenseMatrix[T], optDim: Dimensions2, padValue: T): DenseMatrix[T] = {
//    require( optDim.n1 > 0 && optDim.n2 > 0, "Cannot pad to zero or negative length!")
//    m.cols match {
//      case optDim.n2 => m( *, :: ).map( padRight1ImplFixed(_, Dimensions1(optDim.n1), padValue) )
//      case num: Int if num < optDim.n2 => {
//        DenseMatrix.horzcat(
//            m( *, :: ).map( padRight1ImplFixed(_, Dimensions1(optDim.n1), padValue) ),
//            DenseMatrix.zeros[T](optDim.n1, optDim.n2 - num)
//        )
//      }
//      case num: Int if optDim.n2 < num => m(0 until optDim.n2, ::).map( padRight1ImplFixed(_, Dimensions1(optDim.n1), padValue) )
//      case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
//    }
//  }
//
//  // </editor-fold>

}

object CanPadOpts {

  // <editor-fold defaultstate="collapsed" desc=" OptPadDimensions ">

  sealed abstract class OptPadDimensions extends Opt
  abstract class OptPadDimensions1 extends OptPadDimensions
  abstract class OptPadDimensions2 extends OptPadDimensions
  //This option class specifies a 1 dimensional padding result
  case class Dimensions1(n1: Int) extends OptPadDimensions1
  //This option class specifies a 2 dimensional padding result
  case class Dimensions2(n1: Int, n2: Int) extends OptPadDimensions2

  implicit def intToOptPadDimensions1(n: Int): Dimensions1 = Dimensions1(n)
  implicit def t1ToOptPadDimensions1(n: Tuple1[Int]): Dimensions1 = Dimensions1(n._1)
  implicit def t2ToOptPadDimensions2(n: Tuple2[Int, Int]): Dimensions2 = Dimensions2(n._1, n._2)

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
