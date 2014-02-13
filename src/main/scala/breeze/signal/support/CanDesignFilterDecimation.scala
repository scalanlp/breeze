package breeze.signal.support

import breeze.signal._
import breeze.linalg.DenseVector


/**
 * @author ktakagaki
 * @date 2/4/14.
 */
trait CanDesignFilterDecimation[Output] {
  def apply(factor: Int, multiplier: Double,
            optDesignMethod: OptDesignMethod,
            optWindow: OptWindowFunction, optFilterOrder: OptFilterTaps): FilterKernel1D[Output]
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
  implicit def decimationFilterDouble: CanDesignFilterDecimation[Double] = {
    new CanDesignFilterDecimation[Double] {
      def apply(factor: Int, multiplier: Double,
                optDesignMethod: OptDesignMethod,
                optWindow: OptWindowFunction, optFilterOrder: OptFilterTaps): FIRKernel1D[Double]  =  {

        optDesignMethod match {
          case OptDesignMethod.Firwin => {
            import OptFilterTaps._
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
  implicit def decimationFilterLong: CanDesignFilterDecimation[Long] = {
    new CanDesignFilterDecimation[Long] {
      def apply(factor: Int, multiplier: Double,
                optDesignMethod: OptDesignMethod,
                optWindow: OptWindowFunction, optFilterOrder: OptFilterTaps): FIRKernel1D[Double]  =  {
        designFilterDecimation[Double](factor, multiplier, optDesignMethod, optWindow, optFilterOrder).toLong
      }
    }
  }

}