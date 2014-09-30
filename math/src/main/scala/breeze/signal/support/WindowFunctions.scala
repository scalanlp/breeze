package breeze.signal.support

import breeze.linalg.DenseVector
import scala.math.{cos, Pi}

/**
 * @author ktakagaki
 */
object WindowFunctions {

  def hammingWindow(n: Int, alpha: Double = 0.54, beta: Double = 0.46): DenseVector[Double] = {
    if(n == 1 ) DenseVector(1d)
    else DenseVector.tabulate(n)( count => alpha - beta * cos( 2d * Pi * count / (n - 1)) )
  }

  def blackmanWindow(n:Int, a0: Double = 0.42, a1: Double = 0.5, a2: Double = 0.08) = {
    if(n == 1) DenseVector(1d)
    else DenseVector.tabulate(n)( count => a0 - a1 * cos( 2d * Pi * count / (n - 1)) + a2 * cos( 4d * Pi * count / (n - 1)))
  }

}
