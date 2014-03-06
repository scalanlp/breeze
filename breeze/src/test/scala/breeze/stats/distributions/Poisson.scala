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
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith

import breeze.stats.DescriptiveStats._

@RunWith(classOf[JUnitRunner])
class PoissonTest extends FunSuite with Checkers with MomentsTestBase[Int] with ExpFamTest[Poisson,Int] {
  import Arbitrary.arbitrary
  val expFam = Poisson

  implicit def arbDistr = Arbitrary {
    for(p <- arbitrary[Double].map{_.abs % 200 + 1}) yield new Poisson(p)
  }
  def arbParameter = Arbitrary(arbitrary[Double].map(x => math.abs(x) % 20))
  def paramsClose(p: Double, b: Double) = if(b == 0.0) p < 1E-4 else (p -b).abs / b.abs.max(1E-4) < 1E-1

  def asDouble(x: Int) = x.toDouble
  def fromDouble(x: Double) = x.toInt


  override val VARIANCE_TOLERANCE: Double = 1E-1
  val TOL = 1E-1

  test("cdf") {
    val mean = 5.0
    import breeze.numerics._
    val poi = new Poisson(mean)
    assert( closeTo(poi.cdf(0),exp(-mean)), poi.cdf(0) + " " + exp(-mean) )
  }
}