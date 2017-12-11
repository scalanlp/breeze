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

import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

@RunWith(classOf[JUnitRunner])
class RayleighTest extends FunSuite with Checkers with UnivariateContinuousDistrTestBase with MomentsTestBase[Double] with HasCdfTestBase {
  import org.scalacheck.Arbitrary.arbitrary

  override val numSamples = 40000

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr = Arbitrary {
    for(scale <- arbitrary[Double].map {x => math.abs(x) % 8.0 + 1.0}) yield new Rayleigh(scale)(RandBasis.mt0)
  }

  override type Distr = Rayleigh

  test("#647: 0 outside of support") {
    assert(Rayleigh(1).pdf(0) == 0.0)
    assert(Rayleigh(1).pdf(-1) == 0.0)
  }
}
