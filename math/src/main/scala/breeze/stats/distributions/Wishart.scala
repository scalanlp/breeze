package breeze.stats
package distributions

import breeze.linalg._
import breeze.numerics._

case class Wishart(df: Int, scale: DenseMatrix[Double])(implicit randBasis: RandBasis = Rand)
  extends ContinuousDistr[DenseMatrix[Double]]
  with Moments[DenseMatrix[Double], DenseMatrix[Double]]
{
  private val dims = scale.rows

  require(dims == scale.cols, "Scale must be a square matrix")
  require(df > dims - 1, "df must be greater than one less than the dimensionality")

  private val invScale = inv(scale)
  private val cholScale = cholesky(scale)

  def unnormalizedLogPdf(x: DenseMatrix[Double]): Double = {
    math.log(det(x)) * 0.5 * (df.toDouble - dims - 1) - 0.5 * trace(invScale * x)
  }

  def logNormalizer: Double = {
    (math.log(2) * dims * 0.5 * df
    + math.log(det(scale)) * 0.5 * df
    + multidigammalog(0.5 * df, dims))
  }

  def mean: DenseMatrix[Double] = scale *:* df.toDouble

  def variance: DenseMatrix[Double] = {
    val t = diag(scale).toDenseMatrix
    (mpow(scale, 2) +  t * t.t) *:* df.toDouble
  }

  def entropy: Double = {
    val elnx = multidigamma(df.toDouble / 2, dims) + dims * math.log(2) + math.log(det(scale))
    (-logNormalizer
      - ((df - dims - 1).toDouble / 2) * elnx
      + (df * dims).toDouble / 2)
  }

  def mode: DenseMatrix[Double] = {
    require(df >= dims + 1)
    scale *:* (df.toDouble - dims - 1)
  }

  private val innerMVG = MultivariateGaussian(DenseVector.zeros(dims), scale)
  def draw(): DenseMatrix[Double] = {
    sum(innerMVG.sample(df).map(X => X * X.t))
  }
}
