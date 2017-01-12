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

import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

@RunWith(classOf[JUnitRunner])
class GaussianTest extends FunSuite with Checkers with UnivariateContinuousDistrTestBase with MomentsTestBase[Double] with ExpFamTest[Gaussian,Double] with HasCdfTestBase {
  override type Distr = Gaussian
  val expFam = Gaussian
  import org.scalacheck.Arbitrary.arbitrary;

  def arbParameter = Arbitrary{
    for( mean <- arbitrary[Double].map{_ % 10000.0};
      std <- arbitrary[Double].map{x => math.abs(x) % 8.0 + .1}
    ) yield (mean,std)
  }

  def paramsClose(p: (Double,Double), b: (Double,Double)) = {
    val y1 = (p._1 - b._1).abs / (p._1.abs / 2 + b._1.abs / 2+ 1)  < 1E-1
    val y2 = (p._2 - b._2).abs / (p._2.abs / 2 + b._2.abs / 2+ 1)  < 1E-1
    y1 && y2
  }

  test("Probability of mean") {
    check( Prop.forAll { (m: Double, s: Double)=> (s == 0) || {
        val b = new Gaussian(mu=m,sigma=s.abs);
        b.unnormalizedLogPdf(m) == 0.0;
      }
    })
  }

  test("#295, cdf/inverseCdf broken")  {
    val gaussian = Gaussian(0, 1)
    assert( (gaussian.cdf(gaussian.inverseCdf(0.1)) - 0.1).abs <= 1E-3, gaussian.cdf(gaussian.inverseCdf(0.1)) + " was not close to " + 0.1)
  }

  test("Probability of N(0,1)(1) propto exp(-.5))") {
    assert(new Gaussian(0,1).unnormalizedLogPdf(1.0) === -0.5)
  }

  test ("Gaussian.probability throws an exception when evluating 1.0 < N(0, 1) < 0.0") {
    val thrown = intercept[IllegalArgumentException] {
      new Gaussian(0, 1).probability(1.0, 0.0)
    }
  }

  override val VARIANCE_TOLERANCE: Double = 9E-2

  implicit def arbDistr: Arbitrary[Distr] = Arbitrary {
    for(mean <- arbitrary[Double].map{x => math.abs(x) % 10000.0};
        std <- arbitrary[Double].map {x => math.abs(x) % 8.0 + .1}) yield new Gaussian(mean,std);
  }

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x
}
