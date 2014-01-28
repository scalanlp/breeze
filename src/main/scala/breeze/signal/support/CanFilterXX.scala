package breeze.signal.support

/**CanFilterBP, CanFilterBS, CanFilterLP, CanFilterHP
* @author ktakagaki
*/
import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.signal._


/**
* Construction delegate trait for bandpass filtering type InputType.</p>
* Implementation details (especially
* option arguments) may be added in the future, so it is recommended not
* to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
*
* @author ktakagaki
*/
trait CanFilterBPBS[Input, Output] {
  def apply(data: Input, omega: (Double, Double),
            numtaps: Int, sampleRate: Double, bandStop: Boolean,
            kernelType: OptKernelType,
            overhang: OptOverhang,
            padding: OptPadding): Output
}

/**
* Construction delegate for convolving type InputType.</p>
* Implementation details (especially
* option arguments) may be added in the future, so it is recommended not
* to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
*
* @author ktakagaki
*/
object CanFilterBPBS {

  /** Use via implicit delegate syntax filterBP(x: DenseVector) and filterBS(x: DenseVector)
    *
    */
  implicit val dvDouble1DFilterBPBS : CanFilterBPBS[DenseVector[Double], DenseVector[Double]] = {
    new CanFilterBPBS[DenseVector[Double], DenseVector[Double]] {
      def apply(data: DenseVector[Double], omega: (Double, Double),
                numtaps: Int, sampleRate: Double, bandStop: Boolean,
                kernelType: OptKernelType = OptKernelType.Firwin,
                overhang: OptOverhang,
                padding: OptPadding = OptPadding.Boundary): DenseVector[Double] = {

        val kernel: FIRKernel1D = kernelType match  {
          //case x: OptKernelType.OptDefault => KernelDesign.firwin( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
          case OptKernelType.Firwin =>  KernelDesign.firwin( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
          case x => {
            require(false, "Cannot handle option value "+ x)
            FIRKernel1D(DenseVector[Double](), "null kernel!")
          }
        }

        filter(data, kernel, overhang, padding)
      }
    }
  }




}

