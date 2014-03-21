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

import breeze.stats.DescriptiveStats._
import org.apache.commons.math3.random.MersenneTwister

@RunWith(classOf[JUnitRunner])
class GeometricTest extends FunSuite with Checkers with MomentsTestBase[Int] with ExpFamTest[Geometric,Int] {
  import Arbitrary.arbitrary

  val expFam = Geometric

  override val numSamples = 10000

  override val VARIANCE_TOLERANCE: Double = 1E-1

  def paramsClose(p: Double, q: Double) = {
     (p - q).abs / (p.abs / 2 + q.abs / 2+ 1)  < 1E-1
  }

  implicit def arbParameter = Arbitrary {
    for(p <- arbitrary[Double].map{m => (math.abs(m) % 1.0) + 1E-3}) yield p
  }

  implicit def arbDistr = Arbitrary {
    for(p <- arbitrary[Double].map{m => (math.abs(m) % 1.0) + 1E-3}) yield new Geometric(p)(RandBasis.mt0)
  }

  def asDouble(x: Int) = x.toDouble
  def fromDouble(x: Double) = x.toInt
}
