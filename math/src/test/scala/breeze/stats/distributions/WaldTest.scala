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

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class WaldTest extends FunSuite with Checkers with UnivariateContinuousDistrTestBase with MomentsTestBase[Double] {
  type Distr = Wald
  import Arbitrary.arbitrary
  override val numSamples = 40000

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr: Arbitrary[Distr] = Arbitrary {
    for(location <- arbitrary[Double].map{x => math.abs(x) % 5.0 + 1.1}; // Wald pdf at 0 not defined when location == 1
        scale <- arbitrary[Double].map {x => math.abs(x) % 4.0 + 1.0}) yield new Wald(location,scale)(RandBasis.mt0)
  }



}
