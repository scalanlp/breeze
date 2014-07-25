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

import org.apache.commons.math3.distribution.{WeibullDistribution => ApacheWeibullDistribution}
/**
 * The Weibull-distribution - ratio of two scaled chi^2 variables
 *
 * @author stucchio
*/

class WeibullDistribution(alpha: Double, beta: Double) extends ApacheContinuousDistribution {
  protected final val inner = new ApacheWeibullDistribution(alpha, beta)
}

object WeibullDistribution extends ContinuousDistributionUFuncProvider[Double,WeibullDistribution]
