package breeze.signal.support

/**
 * @author ktakagaki
 */
import breeze.linalg.{DenseVector}
import breeze.signal.filter.{FIRKernel1D, FilterKernel}

/**
 * Construction delegate trait for filtering type InputType.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
 *
 * @author ktakagaki
 */
trait CanFilter[InputType, Kernel, OutputType] {
  def apply(data: InputType, kernel: Kernel,
            optPadding: OptPadding = OptPadding.OptBoundary ): OutputType
}

/**
 * Construction delegate for filtering type InputType.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
 *
 * @author ktakagaki
 */
object CanFilter {

  /** Use via implicit delegate syntax filter(x: DenseVector)
    *
    */
  implicit val dvDouble1DFilter : CanFilter[DenseVector[Double], FIRKernel1D, DenseVector[Double]] = {
    new CanFilter[DenseVector[Double], FIRKernel1D, DenseVector[Double]] {
      def apply(data: DenseVector[Double], kernel: FIRKernel1D,
                optPadding: OptPadding) = {


        optPadding match  {
          case x: OptPadding.OptNone => {
        }

      }
    }
  }


}

//abstract class OptSampleRate
//object OptSampleRate{
//  case class OptDefault() extends OptSampleRate
//  case class OptValue(sampleRate: Double) extends OptSampleRate
//}


