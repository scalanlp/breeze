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
            kernelType: OptKernelDesign,
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
                kernelType: OptKernelDesign = OptKernelDesign.Firwin,
                overhang: OptOverhang,
                padding: OptPadding = OptPadding.Boundary): DenseVector[Double] = {

        val kernel: FIRKernel1D[Double] = kernelType match  {
          //case x: OptKernelType.OptDefault => KernelDesign.firwin( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
          case OptKernelDesign.Firwin =>  firwin[Double]( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
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

