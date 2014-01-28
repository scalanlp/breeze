package breeze.signal.support

import breeze.linalg.{sum, DenseVector, diff}
import breeze.numerics.{cos, sincpi, isOdd, isEven}
import breeze.signal._
import scala.math.{sin, Pi}
import breeze.macros.expand

/**
 * Portions of the code are translated from scipy (scipy.org) based on provisions of the BSD license.
 *
 * @author ktakagaki
 */
abstract class FilterKernel {
  val designText: String
  override def toString = this.getClass.getSimpleName + "(): " + designText
}

abstract class FilterKernel1D extends FilterKernel

/**This immutable class encapsulates 1D FIR filter kernels. It also internally stores the kernel Fourier transform for
  * multiple applications of fft convolution.*/
class FIRKernel1D[T](val kernel: DenseVector[T], override val designText: String) extends FilterKernel1D {
  //lazy val kernelFourier = fourierTransform( kernel )
  lazy val length = kernel.length
}

/**This immutable class will encapsulate 1D IIR kernels. Not implemented yet.*/
class IIRKernel1D(kernelA: DenseVector[Double], kernelB: DenseVector[Double], override val designText: String) extends FilterKernel {
}
