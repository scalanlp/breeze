//package breeze.signal.support
//
///**CanFilterBP, CanFilterBS, CanFilterLP, CanFilterHP
// * @author ktakagaki
// */
//import breeze.linalg.{DenseVector, DenseMatrix}
//import breeze.signal.filter.{FIRKernel1D, KernelDesign}
//import breeze.signal._
//
//
///**
// * Construction delegate trait for bandpass filtering type InputType.</p>
// * Implementation details (especially
// * option arguments) may be added in the future, so it is recommended not
// * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
// *
// * @author ktakagaki
// */
//trait CanFilterBPBS[InputType, OutputType] {
//  def apply(data: DenseVector[Double], omega: (Double, Double),
//            sampleRate: Double, numtaps: Int, bandStop: Boolean,
//            optKernelType: OptKernelType,
//            overhang: OptOverhang,
//            padding: OptPadding = OptPadding.Boundary): OutputType
//}
//
///**
// * Construction delegate for convolving type InputType.</p>
// * Implementation details (especially
// * option arguments) may be added in the future, so it is recommended not
// * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
// *
// * @author ktakagaki
// */
//object CanFilterBPBS {
//
//  /** Use via implicit delegate syntax filterBP(x: DenseVector) and filterBS(x: DenseVector)
//    *
//    */
//  implicit val dvDouble1DFilterBPBS : CanFilterBPBS[DenseVector[Double], DenseVector[Double]] = {
//    new CanFilterBPBS[DenseVector[Double], DenseVector[Double]] {
//      def apply(data: DenseVector[Double], omega: (Double, Double),
//                sampleRate: Double = 1d, numtaps: Int = 512, bandStop: Boolean = false,
//                optKernelType: OptKernelType = OptKernelType.Firwin,
//                overhang: OptOverhang,
//                padding: OptPadding = OptPadding.Boundary): DenseVector[Double] = {
//
//        val kernel = optKernelType match  {
//          case x: OptKernelType.OptDefault => KernelDesign.firwin( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
//          case x: OptKernelType.Firwin =>  KernelDesign.firwin( numtaps, DenseVector[Double](omega._1, omega._2), zeroPass = bandStop, nyquist = sampleRate/2d)
//          case x => require(false, "Cannot handle option value "+ x)
//        }
//
////        val optConvolveOverhang1 = overhang
////        filter(data, kernel, overhang = optConvolveOverhang1, padding = OptPadding.Boundary)
//        DenseVector[Double]()
//      }
//    }
//  }
//
//
//
//
//}
//
