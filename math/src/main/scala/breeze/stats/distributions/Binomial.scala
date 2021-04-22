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

import breeze.numerics.{logI, lgamma}
import math._

/**
 * A binomial distribution returns how many coin flips out of n are heads,
 * where numYes is the probability of any one coin being heads.
 *
 * @author dlwh
 * @param n is the number of coin flips
 * @param p the probability of any one being true
 */
case class Binomial(n: Int, p: Double)(implicit rand: RandBasis = Rand)
    extends DiscreteDistr[Int]
    with Moments[Double, Double] {
  type Distr = Gamma
  require(n > 0, "n must be positive!")
  require(p >= 0.0, "p must be non-negative!")
  def probabilityOf(k: Int) = exp(logProbabilityOf(k))

  override def toString() = "Binomial(" + n + ", " + p + ")"

  override def logProbabilityOf(k: Int) = {
    require(n >= k)
    require(k >= 0)
    if (p == 0) logI(k == 0)
    else if (p == 1) logI(k == n)
    else {
      lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1) + k * log(p) + (n - k) * log(1 - p)
    }
  }

  // faster binomial from NR
  override def draw(): Int = {
    var bnl = 0.0
    if (n < 25) {
      var j = 0
      while (j < n) {
        if (rand.uniform.draw() < pp) bnl += 1
        j += 1
      }
    } else if (np < 1.0) {
      val g = exp(-np)
      var t = 1.0
      var j = 0
      var ok = true
      while (j < n && ok) {
        t *= rand.uniform.draw()
        if (t < g) ok = false
        else j += 1
      }
      bnl = if (j <= n) j else n
    } else {
      var y = 1.0
      var t = 1.0
      var continueOuter = true
      while (continueOuter) {
        var continueInner = true
        while(continueInner) {
          val angle = math.Pi * rand.uniform.draw()
          y = tan(angle)
          bnl = sq * y + np
        }
        continueInner = bnl < 0.0 || bnl >= (n + 1.0)

        bnl = floor(bnl)
        t = 1.2 * sq * (1.0 + y * y) * exp(
          nfact - breeze.numerics.lgamma(bnl + 1.0)
            - breeze.numerics.lgamma(n - bnl + 1.0)
            + bnl * plog + (n - bnl) * pclog
        )

        continueOuter = (rand.uniform.draw() > t)
      }
    }
    if (p != pp) bnl = n - bnl
    bnl.toInt
  }

  private val pp = if (p <= 0.5) p else 1.0 - p
  private val np = n * pp
  // data for the generator {
  private val nfact = breeze.numerics.lgamma(n + 1.0)

  private val pc = 1.0 - pp
  private val plog = log(pp)
  private val pclog = log(pc)

  private val sq = sqrt(2.0 * np * pc)
  //}

  def mean = n * p
  def variance = mean * (1 - p)
  def mode = math.floor((n + 1) * p)

  /** with an additive O(1/n) term */
  def entropy = .5 * math.log(2 * math.Pi * variance)

}
