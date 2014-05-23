package breeze.signal.support

import breeze.linalg.{sum, DenseVector, diff}
import breeze.numerics.{cos, sincpi, isOdd, isEven}
import breeze.signal._
import scala.math.{sin, Pi}
import breeze.math.Complex
import breeze.macros.expand

/**
 * Portions of the code are translated from scipy (scipy.org) based on provisions of the BSD license.
 *
 * @author ktakagaki
 */
abstract class FilterKernel[T] {
  val designText: String
  override def toString = this.getClass.getSimpleName + "(): " + designText
//  def toLong(): FilterKernel[Long]
//  def toInt(): FilterKernel[Int]
//  def toDouble(): FilterKernel[Double]
//  def toFloat(): FilterKernel[Float]
}

abstract class FilterKernel1D[T] extends FilterKernel[T]{
  val multiplier: Double
}

object FIRKernel1D {

  def apply[T](kernel: DenseVector[T], multiplier: Double, designText: String) = new FIRKernel1D[T](kernel, multiplier, designText)

}

/**This immutable class encapsulates 1D FIR filter kernels. It also internally stores the kernel Fourier transform for
  * multiple applications of fft convolution.*/
class FIRKernel1D[T](val kernel: DenseVector[T], override val multiplier: Double, override val designText: String) extends FilterKernel1D[T] {
  //lazy val kernelFourier: DenseVector[Complex] = fourierTr( kernel )
  lazy val length = kernel.length
  /**Amount of overhang to prepend for convolution, to conserve output length.*/
  lazy val overhangPre = (length - 1)/2
  /**Amount of overhang to append for convolution, to conserve output length.*/
  lazy val overhangPost = length - 1 - overhangPre

//  override def toLong(): FIRKernel1D[Long] = FIRKernel1D[Long]( kernel.map(_.toLong), designText )
//  override def toInt(): FIRKernel1D[Int] = FIRKernel1D[Int]( kernel.map(_.toInt), designText )
//  override def toFloat(): FIRKernel1D[Float] = FIRKernel1D[Float]( kernel.map(_.toFloat), designText )
//  override def toDouble(): FIRKernel1D[Double] = FIRKernel1D[Double]( kernel.map(_.toDouble), designText )

  override def toString() = this.getClass.toString + " multiplier: " + multiplier + ": " + designText
}

/**This immutable class will encapsulate 1D IIR kernels. Not implemented yet.*/
class IIRKernel1D[T](val kernelA: DenseVector[T], val kernelB: DenseVector[T], override val multiplier: Double, override val designText: String) extends FilterKernel1D[T] {
}

object IIRKernel1D {
  def apply[T](kernelA: DenseVector[T], kernelB: DenseVector[T]): IIRKernel1D[T] = apply(kernelA, kernelB, s"IIRFilter a=$kernelA b=$kernelB")
  def apply[T](kernelA: DenseVector[T], kernelB: DenseVector[T], designText: String): IIRKernel1D[T] = apply(kernelA, kernelB, 1d, designText)
  def apply[T](kernelA: DenseVector[T], kernelB: DenseVector[T], multiplier: Double, designText: String): IIRKernel1D[T] = new IIRKernel1D[T](kernelA, kernelB, multiplier, designText)
}

/**This immutable class will encapsulate 1D SOS kernels. Not implemented yet.*/
class SOSKernel1D[T](val b0: T, val b1: T, val b2: T, val a1: T, val a2: T, override val multiplier: Double, override val designText: String) extends FilterKernel1D[T] {
}

object SOSKernel1D {
  def apply[T](b0: T, b1: T, b2: T, a1: T, a2: T, designText: String): SOSKernel1D[T] = apply(b0, b1, b2, a1, a2, 1d, designText)
  def apply[T](b0: T, b1: T, b2: T, a1: T, a2: T): SOSKernel1D[T] = apply(b0, b1, b2, a1, a2, s"SOSKernel(b0=$b0, b1=$b1, b2=$b2, a1=$a1, a2=$a2)")
  
  /**
   * allows to specify a custom a0 coeffcient - normaly an un-specified a0 coefficient of 1 is used 
   **/
  def apply[T](b0: T, b1: T, b2: T, a0: T, a1: T, a2: T, multiplier: Double, designText: String)(implicit div: breeze.linalg.operators.OpDiv.Impl2[T, T, T]): SOSKernel1D[T] = new SOSKernel1D[T](div(b0,a0), div(b1, a0), div(b2, a0), div(a1, a0), div(a2, a0), multiplier, designText)
  def apply[T](b0: T, b1: T, b2: T, a1: T, a2: T, multiplier: Double, designText: String): SOSKernel1D[T] = new SOSKernel1D[T](b0, b1, b2, a1, a2, multiplier, designText)
}
