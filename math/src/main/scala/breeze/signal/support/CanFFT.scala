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

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.signal.support.JTransformsSupport._


/**
 * Construction delegate for getting the FFT of a value of type InputType.
 *
 * @author ktakagaki
 */
trait CanFFT[InputType, OutputType] {
  def apply(v1: InputType): OutputType
}

object CanFFT {

  implicit def dvDoubleFFT(v1: DenseVector[Double]) : CanFFT[DenseVector[Double], DenseVector[Complex]] = {
    new CanFFT[DenseVector[Double], DenseVector[Complex]] {
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

  implicit def dvComplexFFT(v1: DenseVector[Complex]) : CanFFT[DenseVector[Complex], DenseVector[Complex]] = {
    new CanFFT[DenseVector[Complex], DenseVector[Complex]] {
      def apply(v: DenseVector[Complex]) = {
        //reformat for input: note difference in format for input to real fft
        val tempArr = denseVectorCToTemp(v)

        //actual action
        val fft_instance = getD1DInstance(v.length)
        fft_instance.complexForward( tempArr ) //does operation in place

        //reformat for output
        tempToDenseVector(tempArr)
      }
    }
  }
}