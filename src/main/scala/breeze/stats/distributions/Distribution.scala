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

import breeze.generic._

/**
 * Represents an unnormalized probability distribution.
 *
 * @author dlwh
 */
trait Density[T] {
  /** Returns the unnormalized value of the measure*/
  def apply(x:T): Double
  /** Returns the log unnormalized value of the measure*/
  def logApply(x:T): Double = apply(x)
}

/**
 * Represents a continuous Distribution.
 * Why T? just in case.
 * @author dlwh
 */
trait ContinuousDistr[T] extends Density[T] with Rand[T] {
  /** Returns the probability density function at that point.*/
  def pdf(x: T): Double  = math.exp(logPdf(x))
  def logPdf(x:T): Double = unnormalizedLogPdf(x) - logNormalizer
  /** Returns the probability density function up to a constant at that point.*/
  def unnormalizedPdf(x:T): Double = math.exp(unnormalizedLogPdf(x))

  def unnormalizedLogPdf(x:T): Double
  val logNormalizer : Double
  lazy val normalizer: Double = math.exp(-logNormalizer) //Needs to be lazy to ensure that it is computed after logNormalizer. Suboptimal I guess.

  def apply(x:T) = unnormalizedPdf(x)
  override def logApply(x:T) = unnormalizedLogPdf(x)
}

trait PdfIsUFunc[U <: UFunc,T,P <: PdfIsUFunc[U,T,P]] { self:P =>
  final def pdf[@specialized(Int, Double, Float) V,
                @specialized(Int, Double, Float) VR]
                (v: V)(implicit impl: UFunc.UImpl2[U,P,V,VR]):VR = impl(self, v)
}

trait ContinuousDistributionUFuncProvider[T,D <: ContinuousDistr[T]] extends UFunc with MappingUFunc { self:UFunc =>
  import breeze.linalg.{DenseVector, DenseMatrix}
  implicit object basicImpl extends Impl2[ContinuousDistrUFuncWrapper,T,Double] { def apply(w: ContinuousDistrUFuncWrapper, v: T) = w.dist.pdf(v) }
  implicit class ContinuousDistrUFuncWrapper(val dist: D) extends PdfIsUFunc[self.type,T,ContinuousDistrUFuncWrapper]
}

/**
 * Represents a discrete Distribution.
 * @author dlwh
 */
trait DiscreteDistr[T] extends Density[T] with Rand[T] {
  /** Returns the probability of that draw. */
  def probabilityOf(x: T): Double
  def logProbabilityOf(x:T): Double = math.log(probabilityOf(x))
  /** Returns the probability of that draw up to a constant */
  def unnormalizedProbabilityOf(x:T): Double = probabilityOf(x)
  def unnormalizedLogProbabilityOf(x:T): Double = math.log(unnormalizedProbabilityOf(x))

  def apply(x:T) = unnormalizedProbabilityOf(x)
  override def logApply(x:T) = unnormalizedLogProbabilityOf(x)
}
