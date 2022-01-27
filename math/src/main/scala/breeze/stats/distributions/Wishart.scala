package breeze.stats
package distributions

import breeze.linalg
import breeze.linalg._
import breeze.macros.cforRange
import breeze.numerics._

case class Wishart(df: Double, scale: DenseMatrix[Double])(implicit randBasis: RandBasis)
  extends ContinuousDistr[DenseMatrix[Double]]
  with Moments[DenseMatrix[Double], DenseMatrix[Double]]
{
  private val dims = scale.rows

  require(dims == scale.cols, "Scale must be a square matrix")
  require(df > dims - 1, "df must be greater than one less than the dimensionality")

  private val invScale = inv(scale)
  private val chol: DenseMatrix[Double] = linalg.cholesky(scale)

  def unnormalizedLogPdf(x: DenseMatrix[Double]): Double = {
    math.log(det(x)) * 0.5 * (df - dims - 1) - 0.5 * trace(invScale * x)
  }

  def logNormalizer: Double = {
    (math.log(2) * dims * 0.5 * df
    + math.log(det(scale)) * 0.5 * df
    + multidigammalog(0.5 * df, dims))
  }

  def mean: DenseMatrix[Double] = scale *:* df

  def variance: DenseMatrix[Double] = {
    val t = diag(scale).toDenseMatrix
    (mpow(scale, 2) +  t * t.t) *:* df
  }

  def entropy: Double = {
    val elnx = multidigamma(df / 2, dims) + dims * math.log(2) + math.log(det(scale))
    (-logNormalizer
      - ((df - dims - 1) / 2) * elnx
      + (df * dims) / 2)
  }

  def mode: DenseMatrix[Double] = {
    require(df >= dims + 1)
    scale *:* (df - dims - 1)
  }

  def draw(): DenseMatrix[Double] = {
    val a = DenseMatrix.zeros[Double](dims, dims)
    cforRange(0 until dims) { i =>
      a(i, i) = math.sqrt(ChiSquared(df - i).draw())
      cforRange(0 until i) { j =>
        a(i, j) = randBasis.gaussian.draw()
      }
    }
    var sample = chol * a
    sample = sample * sample.t
    sample
  }
}
