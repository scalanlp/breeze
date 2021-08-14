/*
 *
 *  Copyright 2014 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

package breeze.stats.distributions

import org.scalacheck.{Arbitrary, Prop}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

trait HasCdfTestBase extends AnyFunSuite with Checkers {
  type Distr <: Density[Double] with Rand[Double] with HasCdf
  implicit def arbDistr: Arbitrary[Distr]

  test("probability gets the same fraction of things as the sampler") {
    check(Prop.forAll { (distr: Distr) =>
      val samples = distr.sample(10000)
      val (low, high) = {
        if (samples(0) < samples(1)) (samples(0), samples(1))
        else (samples(1), samples(0))
      }

      val inRange = samples.count(x => x >= low && x <= high) / (samples.length * 1.0)
      val prob = distr.probability(low, high)
      if (prob >= 0 && math.abs(inRange - prob) <= 2E-2) {
        true
      } else {
        println((inRange, prob))
        false
      }
    })
  }

  test("cdf gets the same fraction of things as the sampler") {
    check(Prop.forAll { (distr: Distr) =>
      val samples = distr.sample(10000)
      val high = samples(0)

      val inRange = samples.count(x => x <= high) / (samples.length * 1.0)
      val prob = distr.cdf(high)
      if (prob >= 0 && math.abs(inRange - prob) <= 2E-2) {
        true
      } else {
        println((inRange, prob))
        false
      }
    })
  }

}
