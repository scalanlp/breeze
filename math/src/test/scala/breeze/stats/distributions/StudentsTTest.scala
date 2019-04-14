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

import org.scalacheck._
import org.scalatest._
import org.scalatestplus.scalacheck._

class StudentsTTest
    extends FunSuite
    with Checkers
    with UnivariateContinuousDistrTestBase
    with MomentsTestBase[Double]
    with HasCdfTestBase {
  import org.scalacheck.Arbitrary.arbitrary

  override val numSamples = 40000

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr = Arbitrary {
    for (dof <- arbitrary[Double].map { x =>
        math.abs(x) % 1000.0 + 3.0
      })
      yield new StudentsT(dof)(RandBasis.mt0)
  }

  override type Distr = StudentsT
}
