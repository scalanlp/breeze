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

import breeze.numerics._
import breeze.optimize.DiffFunction

/**
 * Represents a Poisson random variable.
 * @author dlwh
 */
case class Poisson(mean: Double)(implicit rand: RandBasis=Rand) extends DiscreteDistr[Int] with Moments[Double] {
  require(mean >= 0, "Poisson mean must be non-negative, but got " + mean)
  require(!mean.isInfinite, "Poisson mean must be finite, but got " + mean)

  private val ell = math.exp(-mean)

  override def toString() = "Poisson(" + mean+")"

  // impl from winrand
  def draw():Int = {
    if(mean == 0) 0
    else if(mean < 10.0) { // small
      var t = ell
      var k = 0
      val u = rand.uniform.get
      var s = t
      while(s < u) {
        k += 1
        t *= mean / k
        s += t
      }
      k
    } else {
      val k_start = mean.toInt
      val u = rand.uniform.get
      var t1 = exp(k_start * log(mean) - mean - lgamma(k_start+1))
      if (t1 > u) k_start
      else {
        var k1 = k_start
        var k2 = k_start
        var t2 = t1
        var s = t1
        while(true) {
          k1 += 1
          t1 *= mean / k1; s += t1
          if (s > u) return k1
          if (k2 > 0) {
            t2 *= k2 / mean
            k2 -= 1
            s += t2
            if (s > u) return k2
          }
        }
        sys.error("wtf")
      }

    }
  }

  def probabilityOf(k:Int) = math.exp(logProbabilityOf(k))
  override def logProbabilityOf(k:Int) = {
    -mean + k * log(mean) - lgamma(k+1)
  }

  def cdf(k:Int) = 1 - gammp(k+1, mean)

  def variance = mean
  def mode = math.ceil(mean) - 1

  /** Approximate, slow to compute */
  def entropy = {
    val entr = mean * (1- log(mean))
    var extra = 0.0
    var correction = 0.0
    var k = 0
    var meanmean = 1.0/mean
    do {
      meanmean *= mean
      val ln_k_! = lgamma(k+1)
      correction = meanmean * ln_k_! / exp(ln_k_!)
      extra += correction
      k += 1
    } while(correction > 1E-6)

    entr + exp(-mean) * extra
  }
}


object Poisson extends ExponentialFamily[Poisson,Int] {
  type Parameter = Double
  case class SufficientStatistic(sum: Double, n: Double) extends distributions.SufficientStatistic[SufficientStatistic] {
    def +(t: SufficientStatistic) = SufficientStatistic(t.sum + sum, t.n + n)

    def *(weight: Double) = SufficientStatistic(sum * weight, n * weight)
  }

  def emptySufficientStatistic = SufficientStatistic(0,0)

  def sufficientStatisticFor(t: Int) = SufficientStatistic(t,1)

  def mle(stats: SufficientStatistic) = stats.sum / stats.n

  def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[Double] {
    def calculate(x: Double) = {
      val obj = math.log(x) * stats.sum - x * stats.n
      val grad = stats.sum / x - stats.n
      (-obj,-grad)
    }
  }

  def distribution(p: Poisson.Parameter) = new Poisson(p)
}