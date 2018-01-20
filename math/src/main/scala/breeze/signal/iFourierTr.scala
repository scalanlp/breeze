package breeze.signal

import breeze.generic.UFunc
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.Complex
import breeze.signal.support.JTransformsSupport._

/**Returns the inverse fast fourier transform of a DenseVector or DenseMatrix. Currently,
 * DenseVector/DenseMatrix types of Double and Complex are supported. Scaling
 * follows the common signal processing convention, i.e. no scaling on forward DFT,
 * and <b>1/n scaling for the inverse DFT</b>. Of note, ifft(x: DenseMatrix[Double]) will
 * perform the 2D ifft in both row and column dimensions, as opposed to the MatLab
 * toolbox syntax, which performs column-wise 1D ifft.</p>
 * Implementation is via the implicit trait CanIFFT[ InputType,  OutputType ],
 * which is found in breeze.signal.support.CanIFFT.scala.
 *
 */
object iFourierTr extends UFunc {

  /** Use via implicit delegate syntax ifft(x: DenseVector)
   *
   */
  implicit val dvDoubleIFFT: iFourierTr.Impl[DenseVector[Double], DenseVector[Complex]] = {
    new iFourierTr.Impl[DenseVector[Double], DenseVector[Complex]] {
      def apply(v: DenseVector[Double]) = {
        //reformat for input: note difference in format for input to complex fft
        val tempArr = denseVectorDToTemp(v)

        //actual action
        val fft_instance = getD1DInstance(v.length)
        fft_instance.realInverseFull(tempArr, true) //does operation in place

        //reformat for output
        tempToDenseVector(tempArr)
      }
    }
  }

  /** Use via implicit delegate syntax ifft(x: DenseVector)
   *
   */
  implicit val dvComplexIFFT: iFourierTr.Impl[DenseVector[Complex], DenseVector[Complex]] = {
    new iFourierTr.Impl[DenseVector[Complex], DenseVector[Complex]] {
      def apply(v: DenseVector[Complex]) = {
        //reformat for input: note difference in format for input to real fft
        val tempArr = denseVectorCToTemp(v)

        //actual action
        val fft_instance = getD1DInstance(v.length)
        fft_instance.complexInverse(tempArr, true) //does operation in place

        //reformat for output
        tempToDenseVector(tempArr)
      }
    }
  }

  /** Use via implicit delegate syntax ifft(x: DenseMatrix)
   *
   */
  implicit val dmComplex2DIFFT: iFourierTr.Impl[DenseMatrix[Complex], DenseMatrix[Complex]] = {
    new iFourierTr.Impl[DenseMatrix[Complex], DenseMatrix[Complex]] {
      def apply(v: DenseMatrix[Complex]) = {
        //reformat for input: note difference in format for input to real fft
        val tempMat = denseMatrixCToTemp(v)

        //actual action
        val fft_instance = getD2DInstance(v.rows, v.cols)
        fft_instance.complexInverse(tempMat, true) //does operation in place

        //reformat for output
        tempToDenseMatrix(tempMat, v.rows, v.cols)
      }
    }
  }

  /** Use via implicit delegate syntax ifft(x: DenseMatrix)
   *
   */
  implicit val dmDouble2DIFFT: iFourierTr.Impl[DenseMatrix[Double], DenseMatrix[Complex]] = {
    new iFourierTr.Impl[DenseMatrix[Double], DenseMatrix[Complex]] {
      def apply(v: DenseMatrix[Double]) = {
        //reformat for input
        val tempMat = denseMatrixDToTemp(v)

        //actual action
        val fft_instance = getD2DInstance(v.rows, v.cols)
        fft_instance.complexInverse(tempMat, true) //does operation in place
        //ToDo this could be optimized to use realInverseFull for speed, but only if the indexes are powers of two

        //reformat for output
        tempToDenseMatrix(tempMat, v.rows, v.cols)
      }
    }
  }
}
