package breeze.signal

import breeze.linalg.DenseVector

/**This class is for using breeze.signal functions on Arrays of Double, from Java.
  *
 * @author ktakagaki
 * @date 3/11/14.
 */
object J {

  def convolve(data: Array[Double], kernel: Array[Double]) = breeze.signal.convolve(DenseVector(data), DenseVector(kernel)).toArray

  def filterMedian(data: Array[Double], windowLength: Int) = breeze.signal.filterMedian(DenseVector(data), windowLength).toArray

}
