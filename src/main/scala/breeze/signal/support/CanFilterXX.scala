package breeze.signal.support

/**Implementation for BP, BS, LP and HP filters (CanFilterBPBS, CanFilterLPHP)
* @author ktakagaki
*/
import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.signal._


/**
* Construction delegate trait for bandpass/bandstop filtering type InputType.</p>
* Implementation details (especially
* option arguments) may be added in the future, so it is recommended not
* to call these implicit delegates directly. Instead, use filterBP(x: DenseVector) or filterBS(x: DenseVector).
*
* @author ktakagaki
*/
trait CanFilterBPBS[Input, Output] {
  def apply(data: Input, omega: (Double, Double), taps: Int,
            sampleRate: Double, bandStop: Boolean,
            kernelType: OptDesignMethod,
            overhang: OptOverhang,
            padding: OptPadding): Output
}

/**
 * Construction delegate trait for lowpass/highpass filtering type InputType.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use filterLP(x: DenseVector) or filterHP(x: DenseVector).
 *
 * @author ktakagaki
 */
trait CanFilterLPHP[Input, Output] {
  def apply(data: Input, omega: Double, taps: Int,
            sampleRate: Double, lowPass: Boolean,
            kernelType: OptDesignMethod,
            overhang: OptOverhang,
            padding: OptPadding): Output
}


object CanFilterBPBS {

  /** Use via implicit delegate syntax filterBP(x: DenseVector) and filterBS(x: DenseVector)
    *
    */
  implicit val dvDouble1DFilterBPBS : CanFilterBPBS[DenseVector[Double], DenseVector[Double]] = {
    new CanFilterBPBS[DenseVector[Double], DenseVector[Double]] {
      def apply(data: DenseVector[Double], omega: (Double, Double), taps: Int,
                sampleRate: Double, bandStop: Boolean,
                kernelType: OptDesignMethod,
                overhang: OptOverhang,
                padding: OptPadding): DenseVector[Double] = {

        val kernel: FIRKernel1D[Double] = kernelType match  {
          //case x: OptKernelType.OptDefault => KernelDesign.firwin( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
          case OptDesignMethod.Firwin =>
            designFilterFirwin[Double]( taps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
          case x => {
            require(false, "Cannot handle option value "+ x)
            new FIRKernel1D[Double](DenseVector[Double](), "null kernel!")
          }
        }
//        println(kernel)
//        println(kernel.kernel)

        filter(data, kernel, overhang, padding)
      }
    }
  }




}


object CanFilterLPHP {

  /** Use via implicit delegate syntax filterLP(x: DenseVector) and filterHP(x: DenseVector)
    *
    */
  implicit val dvDouble1DFilterLPHP : CanFilterLPHP[DenseVector[Double], DenseVector[Double]] = {
    new CanFilterLPHP[DenseVector[Double], DenseVector[Double]] {
      def apply(data: DenseVector[Double], omega: Double, taps: Int,
                sampleRate: Double, lowPass: Boolean,
                kernelType: OptDesignMethod,
                overhang: OptOverhang,
                padding: OptPadding): DenseVector[Double] = {

        val kernel: FIRKernel1D[Double] = kernelType match  {
          //case x: OptKernelType.OptDefault => KernelDesign.firwin( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
          case OptDesignMethod.Firwin =>
            designFilterFirwin[Double]( taps, DenseVector[Double]( omega ), zeroPass = lowPass, nyquist = sampleRate/2d)
          case x => {
            require(false, "Cannot handle option value "+ x)
            new FIRKernel1D[Double](DenseVector[Double](), "null kernel!")
          }
        }

        filter(data, kernel, overhang, padding)
      }
    }
  }




}
