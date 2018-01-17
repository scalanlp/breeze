package breeze.stats.distributions;

/*
 Copyright 2017 David Hall, Daniel Ramage

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

/**Tests for MarkovChain
 * @author darrenjw
 * @date 2/1/17
 */
@RunWith(classOf[JUnitRunner])
class MarkovChainTest extends FunSuite with Checkers {
  type Distr = Gamma
  import org.scalacheck.Arbitrary.arbitrary

  import breeze.numerics._
  import breeze.linalg._
  import MarkovChain._

  val numSamples = 1000000
  val tol = 0.01

  test("metropolis for a Gamma") {
    val mc = metropolis(1.0, (x: Double) => Gaussian(x, 1.0))(x => Gamma(2.0, 1.0 / 3).logPdf(x))
    val it = mc.steps
    val its = it.take(numSamples).toArray
    val itsv = DenseVector[Double](its)
    val mav = breeze.stats.meanAndVariance(itsv)
    assert(abs(mav.mean - 2.0 / 3) < tol)
    assert(abs(mav.variance - 2.0 / 9) < tol)
  }

  test("metropolisHastings for a Gamma with a symmetric proposal") {
    val mc = metropolisHastings(1.0, (x: Double) => Gaussian(x, 1.0))(x => Gamma(2.0, 1.0 / 3).logPdf(x))
    val it = mc.steps
    val its = it.take(numSamples).toArray
    val itsv = DenseVector[Double](its)
    val mav = breeze.stats.meanAndVariance(itsv)
    assert(abs(mav.mean - 2.0 / 3) < tol)
    assert(abs(mav.variance - 2.0 / 9) < tol)
  }

  test("metropolisHastings for a Gamma with a non-symmetric proposal") {
    val mc = metropolisHastings(1.0, (x: Double) => Gaussian(x, 1.0 + x))(x => Gamma(2.0, 1.0 / 3).logPdf(x))
    val it = mc.steps
    val its = it.take(numSamples).toArray
    val itsv = DenseVector[Double](its)
    val mav = breeze.stats.meanAndVariance(itsv)
    assert(abs(mav.mean - 2.0 / 3) < tol)
    assert(abs(mav.variance - 2.0 / 9) < tol)
  }

}
