package breeze.signal.filter

import breeze.linalg.DenseVector
import scala.math.{cos, Pi}

/**
 * Created by Kenta on 1/20/14.
 */
object WindowFunction {

  def hammingWindow(n: Int, alpha: Double = 0.54, beta: Double = 0.46): DenseVector[Double] = {
    DenseVector(  (for(count <- 0 until n) yield alpha - beta * cos( 2d * Pi * count / (n - 1))).toArray )
  }

}
