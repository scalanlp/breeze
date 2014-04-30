package breeze.signal

import breeze.linalg._
import breeze.linalg.{sum, DenseMatrix, DenseVector}
import breeze.generic.UFunc
import breeze.math.Complex
import breeze.signal.support.JTransformsSupport._
import breeze.macros.expand
import breeze.numerics.{sin, cos}


/**
 * Returns the discrete fourier transform of a DenseVector or DenseMatrix. Currently,
 * DenseVector/DenseMatrix types of Double and Complex are supported. Scaling
 * follows the common signal processing convention, i.e. <b>no scaling on forward DFT</b>,
 * and 1/n scaling for the inverse DFT. Of note, fft(x: DenseMatrix[Double]) will
 * perform the 2D fft in both row and column dimensions, as opposed to the MatLab
 * toolbox syntax, which performs column-wise 1D fft.</p>
 * Implementation is via the implicit trait fft.Impl[ InputType,  OutputType ],
 * which is found in breeze.signal.support.fft.Impl.scala.
 *
 * @return
 * @author ktakagaki, dlwh
 */
object fourierTr extends UFunc {

  implicit val dvDouble1DFFT : fourierTr.Impl[DenseVector[Double], DenseVector[Complex]] = {
    new fourierTr.Impl[DenseVector[Double], DenseVector[Complex]] {
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

  implicit def dvDT1DFFT_Float: Impl[DenseVector[Float], DenseVector[Complex]] = {
    new Impl[DenseVector[Float], DenseVector[Complex]] {
      def apply(v: DenseVector[Float]) = fourierTr( v.map( _.toDouble) )
    }
  }

  implicit def dvDT1DFFT_Int: Impl[DenseVector[Int], DenseVector[Complex]] = {
    new Impl[DenseVector[Int], DenseVector[Complex]] {
      def apply(v: DenseVector[Int]) = fourierTr( v.map( _.toDouble) )
    }
  }

  implicit def dvDT1DFFT_Long: Impl[DenseVector[Long], DenseVector[Complex]] = {
    new Impl[DenseVector[Long], DenseVector[Complex]] {
      def apply(v: DenseVector[Long]) = fourierTr( v.map( _.toDouble) )
    }
  }

  implicit val dvComplex1DFFT : fourierTr.Impl[DenseVector[Complex], DenseVector[Complex]] = {
    new fourierTr.Impl[DenseVector[Complex], DenseVector[Complex]] {
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

  implicit val dmComplex2DFFT : fourierTr.Impl[DenseMatrix[Complex], DenseMatrix[Complex]] = {
    new fourierTr.Impl[DenseMatrix[Complex], DenseMatrix[Complex]] {
      def apply(v: DenseMatrix[Complex]) = {
        //reformat for input: note difference in format for input to real fft
        val tempMat = denseMatrixCToTemp(v)

        //actual action
        val fft_instance = getD2DInstance(v.rows, v.cols)
        fft_instance.complexForward( tempMat ) //does operation in place

        //reformat for output
        tempToDenseMatrix(tempMat, v.rows, v.cols)
      }
    }
  }

  implicit val dmDouble2DFFT : fourierTr.Impl[DenseMatrix[Double], DenseMatrix[Complex]] = {
    new fourierTr.Impl[DenseMatrix[Double], DenseMatrix[Complex]] {
      def apply(v: DenseMatrix[Double]) = {
        //reformat for input
        val tempMat = denseMatrixDToTemp(v)

        //actual action
        val fft_instance = getD2DInstance(v.rows, v.cols)
        fft_instance.complexForward( tempMat ) //does operation in place
        //ToDo this could be optimized to use realFullForward for speed, but only if the indexes are powers of two

        //reformat for output
        tempToDenseMatrix(tempMat, v.rows, v.cols)
      }
    }
  }


  implicit val dvDouble1DFourierRange: Impl2[DenseVector[Double], Range, DenseVector[Complex]] = {
    new Impl2[DenseVector[Double], Range, DenseVector[Complex]] {
      def apply(v: DenseVector[Double], rangeNegative: Range ): DenseVector[Complex] = {

        val range = rangeNegative.getRangeWithoutNegativeIndexes( v.length )
        //ToDo check lengths and throw errors

        val tempret =
          for( k <- range ) yield {
            val pk2_N = scala.math.Pi * k * 2d / v.length
              sum(DenseVector.tabulate[Complex](v.length)( (n: Int) => {
                                        val nd = n.toDouble
                                        Complex(cos(pk2_N * nd), sin(pk2_N * nd))
                                                                                      } ))
             }

        new DenseVector[Complex]( tempret.toArray[Complex] )

      }
    }
  }

}
