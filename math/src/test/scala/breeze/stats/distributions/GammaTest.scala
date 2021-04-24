package breeze.stats.distributions;

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
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._

class GammaTest
    extends AnyFunSuite
    with Checkers
    with UnivariateContinuousDistrTestBase
    with MomentsTestBase[Double]
    with ExpFamTest[Gamma, Double]
    with HasCdfTestBase {
  type Distr = Gamma
  import org.scalacheck.Arbitrary.arbitrary

  val expFam: Gamma.type = Gamma

  override val numSamples = 40000

  implicit def arbParameter = Arbitrary {
    for (shape <- arbitrary[Double].map { _.abs % 200.0 + 0.2 }; // Gamma pdf at 0 not defined when shape == 1
      scale <- arbitrary[Double].map { _.abs % 8.0 + 1.0 }) yield (shape, scale);
  }

  def paramsClose(p: (Double, Double), b: (Double, Double)) = {
    val y1 = (p._1 - b._1).abs / (p._1.abs / 2 + b._1.abs / 2 + 1) < 2E-1
    val y2 = (p._2 - b._2).abs / (p._2.abs / 2 + b._2.abs / 2 + 1) < 2E-1
    y1 && y2
  }

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr = Arbitrary {
    for (shape <- arbitrary[Double].map { x =>
        math.abs(x) % 1000.0 + 1.1
      }; // Gamma pdf at 0 not defined when shape == 1
      scale <- arbitrary[Double].map { x =>
        math.abs(x) % 8.0 + 1.0
      }) yield new Gamma(shape, scale)(RandBasis.mt0)
  }

  test("Issue #11 on github") {
    val mean = 2.834312
    val meanOfLogs = -0.689661
    val n = 5.000000
    val ss = Gamma.SufficientStatistic(n, meanOfLogs, mean)
    val (k, theta) = Gamma.mle(ss)
  }

  test("logDraw for small values") {
    val g = new Gamma(0.0001, 1)
    val mav = breeze.stats.meanAndVariance(Array.fill(100000)(g.logDraw()).map(math.exp _))
    assert(
      (paramsClose(mav.mean -> mav.variance, g.mean -> g.variance)),
      (mav.mean -> mav.variance) -> (g.mean -> g.variance))
    assert(mav.count == 100000)
  }

  test("endpoint, shape > 1") {
    val g = new Gamma(1.1, 1)
    assert(g.pdf(0.0) == 0)
  }
  test("endpoint, shape == 1") {
    val g = new Gamma(1.0, 1)
    assert(g.pdf(0.0) == 1.0)
  }
  test("endpoint, shape < 1") {
    val g = new Gamma(0.8, 1)
    assert(g.pdf(0.0) == Double.PositiveInfinity)
  }

  test("#631 infinite loop") {
    val ss = Gamma.SufficientStatistic(2.0, 6.170967121146477, 478.64879368949363)
    Gamma.mle(ss)
  }

  test("#762: Gamma") {
    val g = Gamma(1.0,0.1)
    assert(g.pdf(-1.0) == 0.0)
    assert(g.pdf(-1E-6) == 0.0)
  }
}
