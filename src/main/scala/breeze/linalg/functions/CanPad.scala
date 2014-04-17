package breeze.linalg

import breeze.macros.expand
import breeze.linalg.Options._
import breeze.stats.{median, mean}
import scala.reflect.ClassTag
import breeze.linalg.DenseVector

/**
 * @author ktakagaki
 * @date 04/16/2014.
 */
trait CanPadRight[Input, Dimensions, Output] {
  def apply(v: Input, optDim: Dimensions, optMode: OptPadMode): Output
}

object CanPadRight {

  // <editor-fold defaultstate="collapsed" desc=" DenseVector 1D padding ">

  @expand
  @expand.valify
  implicit def implDV_OptPadDim[@expand.args(Int, Long, Float, Double) T]: CanPadRight[DenseVector[T], Dimensions1, DenseVector[T]] =
    new CanPadRight[DenseVector[T], Dimensions1, DenseVector[T]] {
      def apply(v: DenseVector[T], optDim: Dimensions1, optMode: OptPadMode): DenseVector[T] = {
        optMode match {
          case Zero     => padRight1ImplFixed[T](v, optDim, v(0) * 0)
          case Max      => padRight1ImplFixed( v, optDim, max(v) )
          case Min      => padRight1ImplFixed( v, optDim, min(v) )
          case Mean     => padRight1ImplFixed( v, optDim, convert( mean(convert(v, Double)), T)  )  //option "Mean" with Int will return approximate Int mean for padding....
          case Median   => padRight1ImplFixed( v, optDim, convert( median(v), T) )    //option "Median" with Int will return approximate Int median for padding....
          case Value(n: T)    => padRight1ImplFixed( v, optDim, n )

          case Wrap     => padRight1ImplDV( v, optDim, v )
          case Reflect  => padRight1ImplDV( v, optDim, reverse(v) )

          case _ => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
        }
      }
    }

  // </editor-fold>


  // <editor-fold defaultstate="collapsed" desc=" 1D Padding Implementation ">

  def padRight1ImplFixed[@expand.args(Int, Long, Float, Double) T: ClassTag](v: DenseVector[T], optDim: Dimensions1, padValue: T): DenseVector[T] = {
    require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
    v.length match {
      case optDim.n1 => v.copy
      case num: Int if num < optDim.n1 => DenseVector( v.toArray ++ Array.tabulate(optDim.n1 - num)(p => padValue) )
      //ToDo 4: DenseVector.vertcat does not work due to implict problems
      //case num: Int if num < optDim.n1 => DenseVector.vertcat( v, DenseVector.tabulate(optDim.n1 - num)(p => padValue) )
      case num: Int if optDim.n1 < num => v(0 until optDim.n1).copy //function should return a copy
      case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
    }
  }

  def padRight1ImplDV[@expand.args(Int, Long, Float, Double) T: ClassTag](v: DenseVector[T], optDim: Dimensions1, padDV: DenseVector[T]): DenseVector[T] = {
      require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
      require( optDim.n1 - v.length <= padDV.length, "Cannot pad beyond specified padding DenseVector!")
      v.length match {
        case optDim.n1 => v.copy
        case num: Int if num < optDim.n1 => DenseVector( v.toArray ++ padDV.toArray.slice(0, optDim.n1 - num) )
        //ToDo 4: ditto above
        //case num: Int if num < optDim.n1 => DenseVector.vertcat( v, padDV(0 until optDim.n1 - num) )
        case num: Int if optDim.n1 < num => v(0 until optDim.n1).copy //function should return a copy
        case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
      }
  }

  // </editor-fold>


//  // <editor-fold defaultstate="collapsed" desc=" DenseVector 2D padding ">
//
//  implicit def implDM_OptPadDim_OptPadMode[@expand.args(Int, Long, Float, Double) T: ClassTag]:
//                                                          CanPadRight[DenseMatrix[T], Dimensions2, DenseMatrix[T]] =
//  new CanPadRight[DenseMatrix[T], Dimensions2, DenseMatrix[T]] {
//    def apply(m: DenseMatrix[T], optDim: Dimensions2, optMode: OptPadMode): DenseMatrix[T] = {
//      val zero: T = m(0,0)
//      optMode match {
//        case Zero     => padRight2ImplFixed( m, optDim, zero )
////        case Max      => padRight2ImplFixed( m, optDim, max(m) )
////        case Min      => padRight2ImplFixed( m, optDim, min(m) )
////        case Mean     => padRight2ImplFixed( m, optDim, convert( mean(convert(m, Double)), T)  )   //option "Mean" with Int will return approximate Int mean for padding....
////        case Median   => padRight2ImplFixed( m, optDim, median(m) )    //option "Median" with Int will return approximate Int median for padding....
//        case Value(n: T)    => padRight2ImplFixed( m, optDim, n )
//
//        case Wrap     => throw new IllegalArgumentException("Option <Wrap> is not supported for 2D padding.")
//        case Reflect  => throw new IllegalArgumentException("Option <Reflect> is not supported for 2D padding.")
//
//        case _        => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
//      }
//    }
//  }
//
//  // </editor-fold>
//  // <editor-fold defaultstate="collapsed" desc=" 2D Padding Implementation ">
//
//  private def padRight2ImplFixed[@expand.args(Int, Long, Float, Double) T: ClassTag](m: DenseMatrix[T], optDim: Dimensions2, padValue: T): DenseMatrix[T] = {
//    require( optDim.n1 > 0 && optDim.n2 > 0, "Cannot pad to zero or negative length!")
//    m.cols match {
//      case optDim.n2 => DenseMatrix( m( *, :: ).map( padRight1ImplFixed(_, Dimensions1(optDim.n1), padValue).toArray ) )
//      case num: Int if num < optDim.n2 => {
//        DenseMatrix.horzcat(
//            m( *, :: ).map( padRight1ImplFixed(_, Dimensions1(optDim.n1), padValue) ),
//            DenseMatrix.zeros[T](optDim.n1, optDim.n2 - num)
//        )
//      }
//      case num: Int if optDim.n2 < num => m(0 until optDim.n2, *).map( (v: DenseVector[T]) => padRight1ImplFixed( v, Dimensions1(optDim.n1), padValue) )
//      case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
//    }
//  }
//
//  // </editor-fold>

}