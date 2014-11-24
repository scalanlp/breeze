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
import org.apache.commons.math3.random.MersenneTwister

@RunWith(classOf[JUnitRunner])
class ChiSquaredTest extends FunSuite with Checkers with MomentsTestBase[Double] with ExpFamTest[ChiSquared,Double] with HasCdfTestBase {
  type Distr = ChiSquared
  import Arbitrary.arbitrary

  val expFam = ChiSquared

  override val numSamples = 40000

  def arbParameter = Arbitrary {
    for(shape <- arbitrary[Double].map{_.abs % 200.0 + 4.2} ) yield shape
  }

  def paramsClose(p: Double, b: Double) = breeze.numerics.closeTo(p, b, 5E-2)

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr = Arbitrary {
    for(shape <- arbitrary[Double].map{x => math.abs(x) % 1000.0 + 4.2}  ) yield new ChiSquared(shape)(new RandBasis(new MersenneTwister(0))) }

  override val VARIANCE_TOLERANCE: Double = 1E-2

  test("endpoint, k > 2") {
    val g = new ChiSquared(3)
    assert(g.pdf(0.0) == 0)
  }
  test("endpoint, k == 2") {
    val g = new ChiSquared(2.0)
    assert(g.pdf(0.0) == 0.5)
  }
  test("endpoint, k < 2") {
    val g = new ChiSquared(1.5)
    assert(g.pdf(0.0) == Double.PositiveInfinity)
  }
}
