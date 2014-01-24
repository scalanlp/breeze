//package breeze.signal.support
//
///**
// * @author ktakagaki
// */
//import breeze.linalg.{DenseVector}
//import breeze.signal.filter.{FIRKernel1D, FilterKernel}
//import breeze.signal._
//
//
///**
// * Construction delegate trait for filtering type InputType.</p>
// * Implementation details (especially
// * option arguments) may be added in the future, so it is recommended not
// * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
// *
// * @author ktakagaki
// */
//trait CanFilter[InputType, Kernel, OutputType] {
//  def apply(data: InputType, kernel: Kernel,
//            optConvolveOverhang: OptConvolveOverhang,
//            optPadding: OptPadding = OptPadding.OptBoundary ): OutputType
//}
//
///**
// * Construction delegate for filtering type InputType.</p>
// * Implementation details (especially
// * option arguments) may be added in the future, so it is recommended not
// * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
// *
// * @author ktakagaki
// */
//object CanFilter {
//
//  /** Use via implicit delegate syntax filter(x: DenseVector)
//    *
//    */
//  implicit val dvDouble1DFilter : CanFilter[DenseVector[Double], FIRKernel1D, DenseVector[Double]] = {
//    new CanFilter[DenseVector[Double], FIRKernel1D, DenseVector[Double]] {
//      def apply(data: DenseVector[Double], kernel: FIRKernel1D, optConvolveOverhang1: OptConvolveOverhang, optPadding: OptPadding) = {
//
////        optPadding match  {
////          case x: OptPadding.OptNone => {}
////        }
////
////        convolve(data, kernel, cyclical = false, optOverhang = optConvolveOverhang1, optConvolveMethod: OptConvolveMethod)
//        DenseVector[Double]()
//
//      }
//    }
//  }
//
//
//}
//
////abstract class OptSampleRate
////object OptSampleRate{
////  case class OptConvolveDefault() extends OptSampleRate
////  case class OptValue(sampleRate: Double) extends OptSampleRate
////}
//
//
