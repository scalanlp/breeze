package breeze.signal.filter.support

import breeze.linalg.{DenseVector, convert}
import breeze.signal._
import breeze.signal.filter._


/**
 * @author ktakagaki
 * @date 2/4/14.
 */
trait CanDesignFilterDecimation[Output] {
  def apply(factor: Int, multiplier: Double,
            optDesignMethod: OptDesignMethod,
            optWindow: OptWindowFunction, optFilterOrder: OptFilterTaps): Output
}

/**
 * Construction delegate for decimation filter design.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use firwin(x: DenseVector).
 *
 * @author ktakagaki
 */
object CanDesignFilterDecimation {

  /** Use via implicit delegate syntax firwin(xxxx)
    *
    */
  implicit def decimationFilterDouble: CanDesignFilterDecimation[FIRKernel1D[Double]] = {
    new CanDesignFilterDecimation[FIRKernel1D[Double]] {
      def apply(factor: Int, multiplier: Double,
                optDesignMethod: OptDesignMethod,
                optWindow: OptWindowFunction, optFilterOrder: OptFilterTaps): FIRKernel1D[Double]  =  {

        optDesignMethod match {
          case OptDesignMethod.Firwin => {
            import breeze.signal.filter.OptFilterTaps._
            val realOrder = optFilterOrder match {
              case Automatic => 31
              case IntOpt(ord) => ord
            }
            //cannot use parameter-by-name for optWindow, given duplicate variable name
            designFilterFirwin(realOrder, DenseVector( 1d / factor.toDouble ), nyquist = 1d, zeroPass = true, scale = true, multiplier, optWindow)
          }
          case meth: OptDesignMethod => throw new IllegalArgumentException("Design method " + meth + "is not supported yet!")
        }
      }
    }
  }

  /** Use via implicit delegate syntax firwin(xxxx)
    *
    */
  implicit def decimationFilterLong: CanDesignFilterDecimation[FIRKernel1D[Long]] = {
    new CanDesignFilterDecimation[FIRKernel1D[Long]] {
      def apply(factor: Int, multiplier: Double,
                optDesignMethod: OptDesignMethod,
                optWindow: OptWindowFunction, optFilterOrder: OptFilterTaps): FIRKernel1D[Long]  =  {
        val temp = designFilterDecimation[FIRKernel1D[Double]](factor, multiplier, optDesignMethod, optWindow, optFilterOrder)
        temp match {
          case x: FIRKernel1D[Double] => new FIRKernel1D[Long]( convert( x.kernel, Long ), x.multiplier.toLong, x.designText )
          case _ => throw new IllegalArgumentException( "Something is wrong here! ")
        }

      }
    }
  }

}