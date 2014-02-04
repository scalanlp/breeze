package breeze.signal.support

import breeze.linalg.DenseVector
import breeze.signal.{OptFilterOrder, OptDesignMethod, OptWindowFunction}


/**
 * @author ktakagaki
 * @date 2/4/14.
 */
trait CanDesignFilterDecimation[Output] {
  def apply(factor: Int,
            optDesignMethod: OptDesignMethod,
            optWindow: OptWindowFunction, optFilterOrder: OptFilterOrder): FilterKernel1D[Output]
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
      def apply(factor: Int,
                optDesignMethod: OptDesignMethod,
                optWindow: OptWindowFunction, optFilterOrder: OptFilterOrder): FIRKernel1D[Double]  =  {

        optDesignMethod match {
          case meth: OptDesignMethod.Firwin => {
            val realOrder = optFilterOrder match {
              case OptFilterOrder.Automatic => 31
              case OptFilterOrder.Int(ord) => ord
            }
            //cannot use parameter-by-name for optWindow, given duplicate variable name
            designFilterFirwin(realOrder, DenseVector( 1d / factor.toDouble ), nyquist = 1d, zeroPass = true, scale = true, optWindow)
          }
          case meth: OptDesignMethod => throw new IllegalArgumentException("Design method " + meth + "is not supported yet!")
        }
      }
    }
  }


}