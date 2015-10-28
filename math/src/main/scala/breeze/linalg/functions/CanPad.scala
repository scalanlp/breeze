package breeze.linalg

import java.util

import breeze.macros.expand
import breeze.linalg.Options._
import breeze.stats.{median, mean}
import scala.reflect.ClassTag
import breeze.math.Semiring
import spire.implicits.{cforRange, cforRange2}

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
  implicit def implDV_OptPadDim[@expand.args(Int, Long, Float, Double) T: ClassTag: Semiring]: CanPadRight[DenseVector[T], Dimensions1, DenseVector[T]] =
    new CanPadRight[DenseVector[T], Dimensions1, DenseVector[T]] {
      def apply(v: DenseVector[T], optDim: Dimensions1, optMode: OptPadMode): DenseVector[T] = {
        optMode match {
          case Zero     => padRight1ImplZero(  v, optDim )
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

      def padRight1ImplZero[T]
      (v: DenseVector[T], optDim: Dimensions1): DenseVector[T] = {
        padRight1ImplFixed(v, optDim, implicitly[Semiring[T]].zero)
      }

      def padRight1ImplFixed[T](v: DenseVector[T], optDim: Dimensions1, padValue: T): DenseVector[T] = {
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

      def padRight1ImplDV[T](v: DenseVector[T], optDim: Dimensions1, padDV: DenseVector[T]): DenseVector[T] = {
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

    }



  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" DenseVector 2D padding ">

  @expand
  implicit def implDM_OptPadDim_OptPadMode[@expand.args(Int, Long, Float, Double) T: ClassTag: Semiring]: CanPadRight[DenseMatrix[T], Dimensions2, DenseMatrix[T]] =
    new CanPadRight[DenseMatrix[T], Dimensions2, DenseMatrix[T]] {
      def apply(m: DenseMatrix[T], optDim: Dimensions2, optMode: OptPadMode): DenseMatrix[T] = {
        optMode match {
          case Zero     => padRight2ImplZero( m, optDim )
          case Max      => padRight2ImplFixed( m, optDim, max(m) )
          case Min      => padRight2ImplFixed( m, optDim, min(m) )
          case Mean     => padRight2ImplFixed( m, optDim, convert( mean(convert(m.toDenseVector, Double)), T)  )
          case Median   => padRight2ImplFixed( m, optDim, convert( median(convert(m.toDenseVector, Double)), T)  )
          case Value(n: T)    => padRight2ImplFixed( m, optDim, n )

          case Wrap     => throw new IllegalArgumentException("Option <Wrap> is not supported for 2D padding.")
          case Reflect  => throw new IllegalArgumentException("Option <Reflect> is not supported for 2D padding.")

          case _        => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
        }
      }

      def padRight2ImplZero[T]
      (v: DenseMatrix[T], optDim: Dimensions2): DenseMatrix[T] = {
        padRight2ImplFixed(v, optDim, implicitly[Semiring[T]].zero)
      }

      def padRight2ImplFixed[T](m: DenseMatrix[T], optDim: Dimensions2, padValue: T): DenseMatrix[T] = {
        require( optDim.n1 > 0 && optDim.n2 > 0, "Cannot pad to zero or negative length!")

        val tempret = DenseMatrix.zeros[T](optDim.n1, optDim.n2)
        cforRange2(0 until min(optDim.n2, m.cols), 0 until min(optDim.n1, m.rows)){
          (c, r) => tempret(r, c) = m(r, c)
        }
        tempret
      }

    }



  // </editor-fold>

}

trait CanPadLeft[Input, Dimensions, Output] {
  def apply(v: Input, optDim: Dimensions, optMode: OptPadMode): Output
}

object CanPadLeft {

  // <editor-fold defaultstate="collapsed" desc=" DenseVector 1D padding ">

  @expand
  implicit def implDV_OptPadDim[@expand.args(Int, Long, Float, Double) T: ClassTag: Semiring]: CanPadLeft[DenseVector[T], Dimensions1, DenseVector[T]] =
    new CanPadLeft[DenseVector[T], Dimensions1, DenseVector[T]] {
      def apply(v: DenseVector[T], optDim: Dimensions1, optMode: OptPadMode): DenseVector[T] = {
        optMode match {
          case Zero     => padLeft1ImplZero( v, optDim )
          case Max      => padLeft1ImplFixed( v, optDim, max(v) )
          case Min      => padLeft1ImplFixed( v, optDim, min(v) )
          case Mean     => padLeft1ImplFixed( v, optDim, convert( mean(convert(v, Double)), T)  )  //option "Mean" with Int will return approximate Int mean for padding....
          case Median   => padLeft1ImplFixed( v, optDim, convert( median(v), T) )    //option "Median" with Int will return approximate Int median for padding....
          case Value(n: T)    => padLeft1ImplFixed( v, optDim, n )

          case Wrap     => padLeft1ImplDV( v, optDim, v )
          case Reflect  => padLeft1ImplDV( v, optDim, reverse(v) )

          case _ => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
        }
      }

      def padLeft1ImplZero
      (v: DenseVector[T], optDim: Dimensions1): DenseVector[T] = {
        padLeft1ImplFixed(v, optDim, implicitly[Semiring[T]].zero)
      }

      def padLeft1ImplFixed(v: DenseVector[T], optDim: Dimensions1, padValue: T): DenseVector[T] = {
        require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
        v.length match {
          case optDim.n1 => v.copy
          case num: Int if num < optDim.n1 =>
            val res = new Array[T](optDim.n1)
            util.Arrays.fill(res, padValue)
            val r = DenseVector(res)
            r((optDim.n1 - num) until optDim.n1) := v
            r
          case num: Int if optDim.n1 < num => v(v.length - optDim.n1 until v.length).copy //function should return a copy
          case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
        }
      }

      def padLeft1ImplDV[T](v: DenseVector[T], optDim: Dimensions1, padDV: DenseVector[T]): DenseVector[T] = {
        require( optDim.n1 > 0, "Cannot pad to zero or negative length!")
        require( optDim.n1 - v.length <= padDV.length, "Cannot pad beyond specified padding DenseVector!")
        v.length match {
          case optDim.n1 => v.copy
          case num: Int if num < optDim.n1 => DenseVector( reverse(reverse(padDV).apply(0 until optDim.n1 - num)).toArray ++ v.toArray )
          case num: Int if optDim.n1 < num => v(0 until optDim.n1).copy //function should return a copy
          case _ => throw new IllegalArgumentException("(n) specification incorrect: " + optDim.toString + " !")
        }
      }

    }



  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" DenseVector 2D padding ">

  @expand
  implicit def implDM_OptPadDim_OptPadMode[@expand.args(Int, Long, Float, Double) T: ClassTag: Semiring]: CanPadLeft[DenseMatrix[T], Dimensions2, DenseMatrix[T]] =
    new CanPadLeft[DenseMatrix[T], Dimensions2, DenseMatrix[T]] {
      def apply(m: DenseMatrix[T], optDim: Dimensions2, optMode: OptPadMode): DenseMatrix[T] = {
        optMode match {
          case Zero     => padLeft2ImplZero( m, optDim )
          case Max      => padLeft2ImplFixed( m, optDim, max(m) )
          case Min      => padLeft2ImplFixed( m, optDim, min(m) )
          case Mean     => padLeft2ImplFixed( m, optDim, convert( mean(convert(m.toDenseVector, Double)), T)  )
          case Median   => padLeft2ImplFixed( m, optDim, convert( median(convert(m.toDenseVector, Double)), T)  )
          case Value(n: T)    => padLeft2ImplFixed( m, optDim, n )

          case Wrap     => throw new IllegalArgumentException("Option <Wrap> is not supported for 2D padding.")
          case Reflect  => throw new IllegalArgumentException("Option <Reflect> is not supported for 2D padding.")

          case _        => throw new IllegalArgumentException("Option " + optMode.toString + " is not supported!")
        }
      }

      def padLeft2ImplZero[T]
      (v: DenseMatrix[T], optDim: Dimensions2): DenseMatrix[T] = {
        padLeft2ImplFixed(v, optDim, implicitly[Semiring[T]].zero)
      }

      def padLeft2ImplFixed[T](m: DenseMatrix[T], optDim: Dimensions2, padValue: T): DenseMatrix[T] = {
        require( optDim.n1 > 0 && optDim.n2 > 0, "Cannot pad to zero or negative length!")

        val tempret = DenseMatrix.zeros[T](optDim.n1, optDim.n2)
        cforRange2(1 to min(optDim.n2, m.cols), 1 to min(optDim.n1, m.rows)) {
          (c, r) => tempret(optDim.n1 - r , optDim.n2 - c) = m(m.rows - r, m.cols - c)
        }
        tempret
      }

    }



  // </editor-fold>

}