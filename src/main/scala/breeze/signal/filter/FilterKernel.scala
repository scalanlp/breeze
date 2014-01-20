package breeze.signal.filter

import breeze.linalg.DenseVector
import breeze.signal._
import breeze.generic.UFunc
import scala.math.{sin, Pi}

/**
 * Created by Kenta on 1/7/14.
 */
abstract class FilterKernel {
  val designText: String
  override def toString = this.getClass.getSimpleName + "(): " + designText
}

abstract class FilterKernel1D extends FilterKernel

case class FIRKernel1D(val kernel: DenseVector, override val designText: String) extends FilterKernel1D {
  lazy val kernelFourier = fourierTransform(kernel)
  lazy val length = kernel.length
}

case class IIRKernel1D(kernelA: DenseVector[Double], kernelB: DenseVector[Double], override val designText: String) extends FilterKernel {
}

object KernelDesign{

 def firwin(taps: Int, cutoff: DenseVector[Double], window: OptWindowFunction,
            zeroPass: Boolean = true, nyquist: Double = 1d, scale: Boolean = true): FIRKernel1D = {
   require(cutoff.length > 0, "At least one cutoff frequency must be given!")
   require(cutoff.min >= 0, "The cutoff frequencies must be bigger than zero!")
   require(cutoff.max <=nyquist, "The cutoff frequencies must be smaller than the nyquist frequency!")
   require(cutoff.difference )

 }

 val sinc: UFunc[Double, Double] = new UFunc[Double, Double]{
    def apply(x:Double):Double = {
      if(x == 0) 1d else sin(x)/x
    }
 }
 val sincpi: UFunc[Double, Double] = new UFunc[Double, Double]{
   def apply(x:Double):Double = {
     if(x == 0) 1d else {val temp = Pi * x; sin(temp)/(temp)}
   }
 }



 abstract class OptWindowFunction
 object OptWindowFunction{
   case class Hamming(alpha: Double = 0.54, beta: Double = 0.46) extends OptWindowFunction
   case class DenseVector(dv: DenseVector) extends OptWindowFunction
 }

  def difference[Input, Output](data: Input)(implicit canDifference: CanDiff[Input, Output]): Output = canDiff(data)

}

