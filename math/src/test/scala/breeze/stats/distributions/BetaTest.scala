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

import breeze.linalg.{DenseVector, norm}
import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

@RunWith(classOf[JUnitRunner])
class BetaTest extends FunSuite with Checkers with MomentsTestBase[Double] with HasCdfTestBase {
  type Distr = Beta

  val expFam = Beta
  import org.scalacheck.Arbitrary.arbitrary;

  def arbParameter = Arbitrary{
    for( mean <- arbitrary[Double].map{x => math.abs(x) % 100.0 + 1E-4};
      std <- arbitrary[Double].map{x => math.abs(x) % 100 + 1E-4}
    ) yield (mean,std)
  }

  def paramsClose(p: (Double,Double), b: (Double,Double)) = {
    val y1 = (p._1 - b._1).abs / (p._1.abs / 2 + b._1.abs / 2+ 1)  < 1E-1
    val y2 = (p._2 - b._2).abs / (p._2.abs / 2 + b._2.abs / 2+ 1)  < 1E-1
    y1 && y2
  }


  import org.scalacheck.Arbitrary.arbitrary;

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr = Arbitrary {
    for(a <- arbitrary[Double].map{x => math.abs(x) % 10000.0 + 1.1};
        b <- arbitrary[Double].map {x => math.abs(x) % 8.0 + 1.1}) yield new Beta(a,b);
  }

  test("#15 test 1: Small a and b") {
    val a = 0.0014364182264741652
    val b = 0.0024709345620239687
    val n = 1000000
    val samples = new Beta(a,b).sample(n)
    val mean = samples.sum / n
    val true_mean = a / (a+b)
    assert(math.abs(mean - true_mean) < 1e-2, (mean, true_mean))
  }

  test("#15 test 2: Smaller a and b") {
    val a = 7.672385302336129E-4
    val b = 0.5028709732819038
    val n = 100000
    val samples = new Beta(a,b).sample(n)
    val mean = samples.sum / n
    val true_mean = a / (a+b)
    assert(math.abs(mean - true_mean) < 1e-2, (mean, true_mean))
  }

  test("endpoints work") {
    val dist = new Beta(1,2)
    assert(dist.pdf(0) == 2)
    assert(dist.pdf(1) == 0)
  }

  test("Beta.pdf works as a ufunc") {
    val M = 100
    val x = DenseVector.zeros[Double](M)
    val expectedResult = DenseVector.zeros[Double](M)
    var i=0
    while (i < M) {
      x(i) = (1.0/M)*i
      expectedResult(i) = 2.0 - 2.0*x(i)
      i += 1
    }
    val d = new Beta(1,2)
    assert(norm(d.pdf(x) - expectedResult) < 1e-8)
  }

}
