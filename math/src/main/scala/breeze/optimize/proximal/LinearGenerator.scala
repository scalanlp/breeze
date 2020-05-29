package breeze.optimize.proximal

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.optimize.DiffFunction
import breeze.stats.distributions.Rand

/**
 * @author debasish83
 */
object LinearGenerator {
  case class Cost(data: DenseMatrix[Double], labels: DenseVector[Double]) extends DiffFunction[DenseVector[Double]] {
    def calculate(x: DenseVector[Double]) = {
      val cumGradient = DenseVector.zeros[Double](x.length)
      var cumLoss = 0.0
      var i = 0
      while (i < data.rows) {
        val brzData = data(i, ::).t
        val diff = x.dot(brzData) - labels(i)
        cumGradient += brzData * (2.0 * diff)
        cumLoss += diff * diff
        i = i + 1
      }
      (cumLoss, cumGradient)
    }
  }

  def apply(ndim: Int): (DiffFunction[DenseVector[Double]], DenseMatrix[Double], DenseVector[Double]) = {
    val rand = Rand.gaussian(0, 1)
    val data = DenseMatrix.rand[Double](ndim, ndim, rand)
    val labels = DenseVector.rand[Double](ndim, rand).map { x =>
      if (x > 0.5) 1.0 else 0.0
    }
    //||ax - b||_2^{2} = x'a'ax - 2*x'a'*b + c
    val h = (data.t * data) * 2.0
    val q = data.t * labels
    q *= -2.0
    (Cost(data, labels), h, q)
  }
}
