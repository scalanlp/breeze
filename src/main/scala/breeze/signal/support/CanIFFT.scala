package breeze.signal.support

/*
 Copyright 2013 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.Complex
import breeze.signal.support.JTransformsSupport._


/**
 * Construction delegate for getting the IFFT of a value of type InputType.
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call this implicit delegate directly.
 *
 * @author ktakagaki
 */
trait CanIFFT[InputType, OutputType] {
  def apply(v1: InputType): OutputType
}

/**
 * Construction delegate for getting the IFFT of a value of type InputType.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use ifft(x: DenseVector/DenseMatrix).
 *
 * @author ktakagaki
 */
object CanIFFT {

  /** Use via implicit delegate syntax ifft(x: DenseVector)
    *
    */
  implicit val dvDoubleIFFT : CanIFFT[DenseVector[Double], DenseVector[Complex]] = {
    new CanIFFT[DenseVector[Double], DenseVector[Complex]] {
      def apply(v: DenseVector[Double]) = {
        //reformat for input: note difference in format for input to complex fft
        val tempArr = denseVectorDToTemp(v)

        //actual action
        val fft_instance = getD1DInstance(v.length)
        fft_instance.realInverseFull( tempArr, true ) //does operation in place

        //reformat for output
        tempToDenseVector(tempArr)
      }
    }
  }

  /** Use via implicit delegate syntax ifft(x: DenseVector)
    *
    */
  implicit val dvComplexIFFT : CanIFFT[DenseVector[Complex], DenseVector[Complex]] = {
    new CanIFFT[DenseVector[Complex], DenseVector[Complex]] {
      def apply(v: DenseVector[Complex]) = {
        //reformat for input: note difference in format for input to real fft
        val tempArr = denseVectorCToTemp(v)

        //actual action
        val fft_instance = getD1DInstance(v.length)
        fft_instance.complexInverse( tempArr, true ) //does operation in place

        //reformat for output
        tempToDenseVector(tempArr)
      }
    }
  }

  /** Use via implicit delegate syntax ifft(x: DenseMatrix)
    *
    */
  implicit val dmComplex2DIFFT : CanIFFT[DenseMatrix[Complex], DenseMatrix[Complex]] = {
    new CanIFFT[DenseMatrix[Complex], DenseMatrix[Complex]] {
      def apply(v: DenseMatrix[Complex]) = {
        //reformat for input: note difference in format for input to real fft
        val tempMat = denseMatrixCToTemp(v)

        //actual action
        val fft_instance = getD2DInstance(v.rows, v.cols)
        fft_instance.complexInverse( tempMat , true) //does operation in place

        //reformat for output
        tempToDenseMatrix(tempMat, v.rows, v.cols)
      }
    }
  }

  /** Use via implicit delegate syntax ifft(x: DenseMatrix)
    *
    */
  implicit val dmDouble2DIFFT : CanIFFT[DenseMatrix[Double], DenseMatrix[Complex]] = {
    new CanIFFT[DenseMatrix[Double], DenseMatrix[Complex]] {
      def apply(v: DenseMatrix[Double]) = {
        //reformat for input
        val tempMat = denseMatrixDToTemp(v)

        //actual action
        val fft_instance = getD2DInstance(v.rows, v.cols)
        fft_instance.complexInverse( tempMat , true) //does operation in place
        //ToDo this could be optimized to use realInverseFull for speed, but only if the indexes are powers of two

        //reformat for output
        tempToDenseMatrix(tempMat, v.rows, v.cols)
      }
    }
  }

}