package breeze.signal.support

import breeze.signal.{OptWindowFunction}
import breeze.linalg._
import breeze.numerics.{cos, isOdd, isEven, sincpi}
import scala.math.Pi
import breeze.macros.expand

///**
// * Construction delegate trait for convolving type InputType.</p>
// * Implementation details (especially
// * option arguments) may be added in the future, so it is recommended not
// * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
// *
// * @author ktakagaki
// */
//trait CanIIR[Output] {
//  def apply(cutOff: Double, order: Int, flavour: OptFilterFlavour = OptFilterFlavour.LoPass): IIRKernel1D[Output]
//}
//
///**
// * Construction delegate for iir filter design.</p>
// * Implementation details (especially
// * option arguments) may be added in the future, so it is recommended not
// * to call these implicit delegates directly. Instead, use firwin(x: DenseVector).
// *
// * @author ktakagaki
// */
//object CanIIR {
//
//
//  implicit def iirDouble: CanIIR[Double] = {
//    new CanIIR[Double] {
//      def apply(cutOff: Double, order: Int, flavour: OptFilterFlavour = OptFilterFlavour.LoPass): IIRKernel1D[Double]
//      =  new IIRKernel1D[Double](
//          // TODO: implement me
//          DenseVector.zeros(), DenseVector.zeros(),
//          s"IIRKernel1D(firwin): cuttOff=$cutOff, order=$order, flavour=$flavour"
//      )
//
//    }
//  }
//
//  @expand
//  implicit def iirT[@expand.args(Int, Long, Float) T]: CanIIR[T] = {
//    new CanIIR[T] {
//      def apply(cutOff: Double, order: Int, flavour: OptFilterFlavour = OptFilterFlavour.LoPass): IIRKernel1D[T]
//      =  new IIRKernel1D[T](
//          // TODO: implement me
//    	DenseVector.zeros(), DenseVector.zeros(),
//        s"IIRKernel1D(firwin): cuttOff=$cutOff, order=$order, flavour=$flavour"
//      )
//    }
//  }
//
//}
