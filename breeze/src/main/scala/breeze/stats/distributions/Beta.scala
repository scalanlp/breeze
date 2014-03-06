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

/**
 * The Beta distribution, which is the conjugate prior for the Bernoulli distribution
 *
 * @author dlwh
 * @param a the number of pseudo-observations for true
 * @param b the number of pseudo-observations for false
 */
class Beta(a: Double, b: Double)(implicit rand: RandBasis = Rand) extends ContinuousDistr[Double]  with Moments[Double, Double] {
  require(a > 0.0)
  require(b > 0.0)

  override def unnormalizedLogPdf(x: Double) = {
    require(x >= 0)
    require(x <= 1)
    (a-1) * log(x) + (b-1) * log(1-x)
  }

  override def pdf(x: Double): Double = {
    require(x >= 0)
    require(x <= 1)
    x match {
      case 0.0 => if (a > 1) { 0 } else if (a == 1) { normalizer } else { Double.PositiveInfinity }
      case 1.0 => if (b > 1) { 0 } else if (b == 1) { normalizer } else { Double.PositiveInfinity }
      case x => math.exp(logPdf(x))
    }
  }

  lazy val logNormalizer =  lgamma(a) + lgamma(b) - lgamma(a+b)

  private val aGamma = new Gamma(a,1)(rand)
  private val bGamma = new Gamma(b,1)(rand)

  override def draw():Double = {
    // from tjhunter, a corrected version of numpy's rk_beta sampling in mtrand/distributions.c
    if(a <= .5 && b <= .5) {
      while (true) {
        val U = rand.uniform.draw()
        val V = rand.uniform.draw()
        if (U > 0 && V > 0) {
          // Performing the computations in the log-domain
          // The exponentiation may fail if a or b are really small
          //        val X = math.pow(U, 1.0 / a)
          val logX = math.log(U) / a
          //        val Y = math.pow(V, 1.0 / b)
          val logY=  math.log(V) / b
          val logSum = softmax(logX, logY)
          if (logSum <= 0.0) {
            return math.exp(logX - logSum)
          }
        } else {
          throw new RuntimeException("Underflow!")
        }
      }
      throw new RuntimeException("Shouldn't be here.")
    } else if(a <= 1 && b <= 1) {
      while (true) {
        val U = rand.uniform.draw()
        val V = rand.uniform.draw()
        if (U > 0 && V > 0) {
          // Performing the computations in the log-domain
          // The exponentiation may fail if a or b are really small
          val X = math.pow(U, 1.0 / a)
          val Y = math.pow(V, 1.0 / b)
          val sum = X + Y
          if (sum <= 1.0) {
            return X / sum
          }
        } else {
          throw new RuntimeException("Underflow!")
        }
      }
      throw new RuntimeException("Shouldn't be here.")
    } else {
      val ad = aGamma.draw()
      val bd = bGamma.draw()
      ad / (ad + bd)
    }
  }

  def mean = a / (a + b)
  def variance = (a * b) / ( (a + b) * (a+b) * (a+b+1))
  def mode = (a - 1) / (a+b - 2)
  def entropy = logNormalizer - (a - 1) * digamma(a) - (b-1) * digamma(b) + (a + b - 2) * digamma(a + b)
}

object Beta extends ExponentialFamily[Beta,Double] with ContinuousDistributionUFuncProvider[Double,Beta] {
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
    import breeze.linalg.DenseVector.TupleIsomorphisms._
    val lensed = likelihoodFunction(stats).throughLens[DenseVector[Double]]
    val startingA = stats.meanLog.abs // MoM would include variance, meh.
    val startingB = stats.meanLog1M.abs // MoM would include variance, meh
    val result = minimize(lensed,DenseVector(startingA,startingB))
    val res@(a,b) = (result(0),result(1))
    res
  }

  def distribution(ab: Parameter) = new Beta(ab._1,ab._2)

  def likelihoodFunction(stats: SufficientStatistic):DiffFunction[(Double,Double)] = new DiffFunction[(Double,Double)]{
    import stats.n
    def calculate(x: (Double, Double)) = {
      val (a,b) = x
      if(a < 0 || b < 0) (Double.PositiveInfinity,(0.0,0.0))
      else {
        val obj = n * (lgamma(a) + lgamma(b) - lgamma(a+b) - (a-1)*stats.meanLog - (b-1) *stats.meanLog1M)
        val gradA = n * (digamma(a) - digamma(a+b) - stats.meanLog)
        val gradB = n * (digamma(b) - digamma(a+b) - stats.meanLog1M)
        (obj,(gradA,gradB))
      }
    }
  }
}
