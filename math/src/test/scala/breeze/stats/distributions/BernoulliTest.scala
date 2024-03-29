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

import org.scalatest._
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._
import org.scalacheck._

import breeze.numerics._

class BernoulliTest extends RandTestBase with MomentsTestBase[Boolean] with ExpFamTest[Bernoulli, Boolean] {
  type Distr = Bernoulli
  val expFam: Bernoulli.type = Bernoulli

  import Arbitrary.arbitrary

  override val numSamples: Int = 30000

  def arbParameter = Arbitrary(arbitrary[Double].map(x => math.max(math.abs(x) % 1.0, 1E-1)))

  def paramsClose(p: Double, b: Double) = if (b == 0.0) p < 1E-4 else (p - b).abs / b.abs.max(1E-4) < 1E-1

  implicit def arbDistr = Arbitrary {
    // make scala 2 happy
    implicit val basis: RandBasis = RandBasis.mt0
    for (p <- arbitrary[Double].map { x =>
        math.max(math.abs(x) % 1.0, 1E-1)
      }) yield new Bernoulli(p)
  }

  def asDouble(x: Boolean) = I(x)
  def fromDouble(x: Double) = x != 0.0

}
