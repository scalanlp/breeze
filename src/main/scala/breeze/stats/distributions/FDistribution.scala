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

import math._

import breeze.numerics.{lgamma,digamma}
import breeze.linalg._
import breeze.optimize._
import breeze.numerics
import org.apache.commons.math3.distribution.{FDistribution => ApacheFDistribution, RealDistribution => ApacheRealDistribution}
/**
 * The F-distribution - ratio of two scaled chi^2 variables
 *
 * @author stucchio
*/

class FDistribution(numeratorDegreesOfFreedom: Double, denominatorDegreesOfFreedom: Double) extends ApacheContinuousDistribution /* with Moments[Double,Double] */ {
  //Moments not implemented cause I can't find the entropy of it
  protected final val inner = new ApacheFDistribution(numeratorDegreesOfFreedom, denominatorDegreesOfFreedom)
  def mode = ((numeratorDegreesOfFreedom-2)/numeratorDegreesOfFreedom)*(denominatorDegreesOfFreedom/(denominatorDegreesOfFreedom+2))
}

object FDistribution extends ContinuousDistributionUFuncProvider[Double,Beta]
