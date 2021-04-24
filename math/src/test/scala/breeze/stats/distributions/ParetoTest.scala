package breeze.stats.distributions

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
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

class ParetoTest
    extends AnyFunSuite
    with Checkers
    with UnivariateContinuousDistrTestBase
    with MomentsTestBase[Double]
    with HasCdfTestBase {
  import Arbitrary.arbitrary

  override val numSamples = 40000

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr: Arbitrary[Pareto] = Arbitrary {
    for (location <- arbitrary[Double].map { x =>
        math.abs(x) % 1000.0 + 1.1
      }; // Pareto pdf at 0 not defined when location == 1
      scale <- arbitrary[Double].map { x =>
        math.abs(x) % 8.0 + 4.0
      }) yield Pareto(location, scale)(RandBasis.withSeed(0))
  }

  override type Distr = Pareto
}
