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
//            overhang: OptOverhang,
//            padding: OptPadding = OptPadding.Boundary ): OutputType
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
//      def apply(data: DenseVector[Double], kernel: FIRKernel1D, optConvolveOverhang1: OptOverhang, padding: OptPadding) = {
//
////        padding match  {
////          case x: OptPadding.None => {}
////        }
////
////        convolve(data, kernel, cyclical = false, overhang = optConvolveOverhang1, method: OptMethod)
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
////  case class Value(sampleRate: Double) extends OptSampleRate
////}
//
//
