package breeze.linalg.support


import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.math.Complex
import breeze.signal.support.JTransformsSupport._


/**
 * Construction delegate for difference.
 *
 * @author ktakagaki
 */
trait CanDiff[InputType, OutputType] {
  def apply(v1: InputType): OutputType
}

object CanDiff {

  /** Use via implicit delegate syntax difference(x: DenseVector)
    *
    */
  implicit val dvDouble1DDiff : CanDiff[DenseVector[Double], DenseVector[Double]] = {
    new CanDiff[DenseVector[Double], DenseVector[Double]] {
      def apply(v: DenseVector[Double]) = {
        //reformat for input: note difference in format for input to complex fft
        val tempArr = denseVectorDToTemp(v)

        //actual action
        val fft_instance = getD1DInstance(v.length)
        fft_instance.realForwardFull( tempArr ) //does operation in place

        //reformat for output
        tempToDenseVector(tempArr)
      }
    }
  }


}