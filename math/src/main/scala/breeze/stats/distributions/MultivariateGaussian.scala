package breeze.stats.distributions

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.numerics._
import math.{Pi, log1p}
import breeze.linalg._
import scala.runtime.ScalaRunTime

/**
 * Represents a Gaussian distribution over a single real variable.
 *
 * @author dlwh
 */
case class MultivariateGaussian(mean: DenseVector[Double], covariance: DenseMatrix[Double])(implicit
    rand: RandBasis = Rand
) extends ContinuousDistr[DenseVector[Double]]
    with Moments[DenseVector[Double], DenseMatrix[Double]] {
  def draw() = {
    val z: DenseVector[Double] = DenseVector.rand(mean.length, rand.gaussian(0, 1))
    root * z += mean
  }

  private val root: DenseMatrix[Double] = cholesky(covariance)

  override def toString() = ScalaRunTime._toString(this)

  override def unnormalizedLogPdf(t: DenseVector[Double]) = {
    val centered = t - mean
    val slv = covariance \ centered

    -slv.dot(centered) / 2.0

  }

  override lazy val logNormalizer = {
    // determinant of the cholesky decomp is the sqrt of the determinant of the cov matrix
    // this is the log det of the cholesky decomp
    val det = sum(log(diag(root)))
    mean.length / 2.0 * log(2 * Pi) + det
  }

  def variance = covariance
  def mode = mean
  lazy val entropy = {
    mean.length * log1p(2 * Pi) + sum(log(diag(root)))
  }
}
