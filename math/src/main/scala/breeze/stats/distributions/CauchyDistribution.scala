package breeze.stats
package distributions
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

import org.apache.commons.math3.distribution.{CauchyDistribution => ApacheCauchyDistribution}
import org.apache.commons.math3.random.RandomGenerator

/**
 * The Cauchy-distribution
 *
 * @author stucchio
 */
case class CauchyDistribution(median: Double, scale: Double)(implicit rand: RandBasis = Rand)
    extends ApacheContinuousDistribution {
  val rng: RandomGenerator = rand.generator
  val inverseCumAccuracy: Double = ApacheCauchyDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY
  protected final val inner = new ApacheCauchyDistribution(rng, median, scale, inverseCumAccuracy)
}

object CauchyDistribution extends ContinuousDistributionUFuncProvider[Double, CauchyDistribution]
