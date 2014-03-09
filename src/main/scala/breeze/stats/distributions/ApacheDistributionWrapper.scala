package breeze.stats
package distributions

/*
 Copyright 2014 Chris Stucchio

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

import org.apache.commons.math3.distribution.{FDistribution => ApacheFDistribution, AbstractRealDistribution => ApacheRealDistribution}
import math._

trait WrapsInnerDistribution {
  protected val inner: ApacheRealDistribution
}
trait ApacheContinuousDistribution extends ContinuousDistr[Double] with WrapsInnerDistribution with HasCdf with HasInverseCdf {
  def unnormalizedLogPdf(x: Double) = math.log(inner.density(x))
  override def pdf(x: Double) = inner.density(x)
  lazy val logNormalizer = 1.0
  def draw() = inner.sample()
  def drawMany(n: Int): Array[Double] = inner.sample(n)
  def probability(x: Double, y: Double) = inner.probability(x,y)
  def inverseCdf(p: Double) = inner.inverseCumulativeProbability(p)
  def mean: Double = inner.getNumericalMean()
  def variance: Double = inner.getNumericalVariance()
}
