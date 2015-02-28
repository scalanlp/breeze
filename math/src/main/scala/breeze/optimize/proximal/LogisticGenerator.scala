package breeze.optimize.proximal

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.optimize.DiffFunction
import breeze.stats.distributions.Rand

/**
 * @author debasish83
 */
object LogisticGenerator {

  case class Cost(data: DenseMatrix[Double],
                  labels: DenseVector[Double]) extends DiffFunction[DenseVector[Double]] {
    def calculate(x: DenseVector[Double]) = {
      val cumGradient = DenseVector.zeros[Double](x.length)
      var cumLoss = 0.0

      var i = 0
      while (i < data.rows) {
        val brzData = data(i, ::).t
        val margin: Double = -1.0 * x.dot(brzData)
        val gradientMultiplier = (1.0 / (1.0 + math.exp(margin))) - labels(i)
        val gradient = brzData * gradientMultiplier
        val loss =
          if (labels(i) > 0) {
            math.log1p(math.exp(margin)) // log1p is log(1+p) but more accurate for small p
          } else {
            math.log1p(math.exp(margin)) - margin
          }
        cumGradient += gradient
        cumLoss += loss
        i = i + 1
      }
      (cumLoss, cumGradient)
    }
  }

  def apply(ndim: Int): DiffFunction[DenseVector[Double]] = {
    val rand = Rand.gaussian(0, 1)
    val data = DenseMatrix.rand[Double](ndim, ndim, rand)
    val labels = DenseVector.rand[Double](ndim, rand).map { x => if (x > 0.5) 1.0 else 0.0}
    Cost(data, labels)
  }
}