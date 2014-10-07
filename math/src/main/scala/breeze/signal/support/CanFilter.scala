package breeze.signal.support

/**
* @author ktakagaki
*/
import breeze.linalg.{DenseVector}
import breeze.signal._


/**
* Construction delegate trait for filtering type InputType.</p>
* Implementation details (especially
* option arguments) may be added in the future, so it is recommended not
* to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
*
* @author ktakagaki
*/
trait CanFilter[Input, KernelType, Output] {
  def apply(data: Input, kernel: KernelType,
            overhang: OptOverhang,
            padding: OptPadding ): Output
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
  implicit val dvDouble1DFilter : CanFilter[DenseVector[Double], FIRKernel1D[Double], DenseVector[Double]] = {
    new CanFilter[DenseVector[Double], FIRKernel1D[Double], DenseVector[Double]] {
      def apply(data: DenseVector[Double], kernel: FIRKernel1D[Double],
                overhang: OptOverhang,
                padding: OptPadding): DenseVector[Double] = {
        convolve(data, kernel.kernel, OptRange.All, overhang, padding)
      }
    }
  }

  /** Use via implicit delegate syntax filter(x: DenseVector)
    *
    */
  implicit val dvInt1DFilter : CanFilter[DenseVector[Int], FIRKernel1D[Int], DenseVector[Int]] = {
    new CanFilter[DenseVector[Int], FIRKernel1D[Int], DenseVector[Int]] {
      def apply(data: DenseVector[Int], kernel: FIRKernel1D[Int],
                overhang: OptOverhang,
                padding: OptPadding): DenseVector[Int] = {
        convolve(data, kernel.kernel, OptRange.All, overhang, padding)
      }
    }
  }

  /** Use via implicit delegate syntax filter(x: DenseVector)
    *
    */
  implicit val dvDouble1DFilterVectorKernel : CanFilter[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    new CanFilter[DenseVector[Double], DenseVector[Double], DenseVector[Double]] {
      def apply(data: DenseVector[Double], kernel: DenseVector[Double],
                overhang: OptOverhang,
                padding: OptPadding): DenseVector[Double] = {
        convolve(data, kernel, /*new FIRKernel1D(kernel, "User-specified kernel"),*/ OptRange.All, overhang, padding)
      }
    }
  }

//  /**
//   * IIRFilter delegate implementation
//   */
//  implicit val dvDoubleIIRFilterKernel : CanFilter[DenseVector[Double], IIRKernel1D[Double], DenseVector[Double]] = {
//    new CanFilter[DenseVector[Double], IIRKernel1D[Double], DenseVector[Double]] {
//      def apply(data: DenseVector[Double], kernel: IIRKernel1D[Double],
//                overhang: OptOverhang,
//                padding: OptPadding): DenseVector[Double] = {
//        // TODO: apply IIRFilter to data vector
//      }
//    }
//  }
//
//  /**
//   * SOSFilter delegate implementation
//   */
//  implicit val dvDoubleSOSFilterKernel : CanFilter[DenseVector[Double], SOSKernel1D[Double], DenseVector[Double]] = {
//    new CanFilter[DenseVector[Double], SOSKernel1D[Double], DenseVector[Double]] {
//      def apply(data: DenseVector[Double], kernel: SOSKernel1D[Double],
//                overhang: OptOverhang,
//                padding: OptPadding): DenseVector[Double] = {
//        // TODO: apply SOSFilter to data vector
//      }
//    }
//  }


}