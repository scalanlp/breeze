package breeze.signal.support

import breeze.linalg.{sum, DenseVector, diff}
import breeze.numerics.{cos, sincpi, isOdd, isEven}
import breeze.signal._
import scala.math.{sin, Pi}
import breeze.math.Complex

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
  val multiplier: T
}

object FIRKernel1D {

  def apply[T](kernel: DenseVector[T], multiplier: T, designText: String) = new FIRKernel1D[T](kernel, multiplier, designText)

}

/**This immutable class encapsulates 1D FIR filter kernels. It also internally stores the kernel Fourier transform for
  * multiple applications of fft convolution.*/
class FIRKernel1D[T](val kernel: DenseVector[T], override val multiplier: T, override val designText: String) extends FilterKernel1D[T] {
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
class IIRKernel1D[T](val kernelA: DenseVector[T], val kernelB: DenseVector[T], override val multiplier: T, override val designText: String) extends FilterKernel1D[T] {
}
