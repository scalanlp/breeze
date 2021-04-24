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

class BinomialTest extends AnyFunSuite with Checkers with MomentsTestBase[Int] {
  type Distr = Binomial
  import org.scalacheck.Arbitrary.arbitrary;

  override val numSamples: Int = 100000
  override val VARIANCE_TOLERANCE: Double = 1E-1

  implicit def arbDistr = Arbitrary {
    for (n <- arbitrary[Int].map { _.abs % 10000 + 1 };
      p <- arbitrary[Double].map { _.abs % 1.0 + 1E-4 }) yield new Binomial(n.abs + 1, p)
  }

  def asDouble(x: Int): Double = x.toDouble
  def fromDouble(x: Double): Int = x.toInt

  test("gh issues/282") {
    val b1 = Binomial(10, 1)
    val b0 = Binomial(10, 0)
    assert(b1.probabilityOf(10) === 1.0)
    assert(b1.probabilityOf(0) === 0.0)
    assert(b0.probabilityOf(1) === 0.0)
    assert(b0.probabilityOf(0) === 1.0)
    for (i <- 1 until 10) {
      assert(b1.probabilityOf(i) === 0.0)
    }
  }
}
