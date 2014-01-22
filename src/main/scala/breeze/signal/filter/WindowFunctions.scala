package breeze.signal.filter

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

}
