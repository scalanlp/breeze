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
import breeze.optimize.{DiffFunction, LBFGS}

/**
 * The Beta distribution, which is the conjugate prior for the Bernoulli distribution
 *
 * @author dlwh
 * @param a the number of pseudo-observations for false
 * @param b the number of pseudo-observations for true
 */
class Beta(a: Double, b: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double]  with Moments[Double] {
  require(a > 0.0)
  require(b > 0.0)

  override def unnormalizedLogPdf(x: Double) = {
    require(x >= 0)
    require(x <= 1)
    (a-1) * log(x) + (b-1) * log(1-x)
  }
  
  val logNormalizer =  lgamma(a) + lgamma(b) - lgamma(a+b)
  
  private val aGamma = new Gamma(a,1)(rand)
  private val bGamma = new Gamma(b,1)(rand)
  
  override def draw() = {
    val ad = aGamma.get
    val bd = bGamma.get
    (ad) / (ad + bd)
  }
  
  def mean = a / (a + b)
  def variance = (a * b) / ( (a + b) * (a+b) * (a+b+1))
  def mode = (a - 1) / (a+b - 2)
  def entropy = logNormalizer - (a - 1) * digamma(a) - (b-1) * digamma(b) + (a + b - 2) * digamma(a + b)
}

object Beta extends ExponentialFamily[Beta,Double] {
  type Parameter = (Double,Double)
  case class SufficientStatistic(n: Double, meanLog: Double, meanLog1M: Double) extends distributions.SufficientStatistic[SufficientStatistic]  {
    def *(weight: Double) = SufficientStatistic(n*weight,meanLog, meanLog1M)
    def +(t: SufficientStatistic) = {
      val delta = t.meanLog - meanLog
      val newMeanLog = meanLog + delta * (t.n /(t.n + n))
      val logDelta = t.meanLog1M - meanLog1M
      val newMeanLog1M = meanLog1M + logDelta * (t.n /(t.n + n))
      SufficientStatistic(n+t.n, newMeanLog, newMeanLog1M)
    }
  }

  def emptySufficientStatistic = SufficientStatistic(0,0,0)

  def sufficientStatisticFor(t: Double) = SufficientStatistic(1,math.log(t),math.log1p(-t))

  def mle(stats: SufficientStatistic): (Double, Double) = {
    val lensed = likelihoodFunction(stats).throughLens[DenseVector[Double]]
    val lbfgs = new LBFGS[DenseVector[Double]](200,3)
    val startingA = stats.meanLog.abs // MoM would include variance, meh.
    val startingB = stats.meanLog1M.abs // MoM would include variance, meh
    val result = lbfgs.minimize(lensed,DenseVector(startingA,startingB))
    val res@(a,b) = (result(0),result(1))
    res
  }

  def distribution(ab: Parameter) = new Beta(ab._1,ab._2)

  def likelihoodFunction(stats: SufficientStatistic):DiffFunction[(Double,Double)] = new DiffFunction[(Double,Double)]{
    import stats.n
    def calculate(x: (Double, Double)) = {
      val (a,b) = x
      if(a < 0 || b < 0) (Double.PositiveInfinity,(0.,0.))
      else {
        val obj = n * (lgamma(a) + lgamma(b) - lgamma(a+b) - (a-1)*stats.meanLog - (b-1) *stats.meanLog1M)
        val gradA = n * (digamma(a) - digamma(a+b) - stats.meanLog)
        val gradB = n * (digamma(b) - digamma(a+b) - stats.meanLog1M)
        (obj,(gradA,gradB))
      }
    }
  }
}
