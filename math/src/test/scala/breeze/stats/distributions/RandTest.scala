/*
 *
 *  Copyright 2015 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

package breeze.stats.distributions

import org.scalatest.funsuite.AnyFunSuite

class RandTest extends AnyFunSuite {
  test("randInt is always non-negative") {
    assert(RandBasis.mt0.randInt.sample(10000).forall(_ >= 0))
    assert(RandBasis.mt0.randLong.sample(10000).forall(_ >= 0))
  }

  test("RandBasis.withSeed ensures distinct seeds in different threads") {
    implicit val basis: RandBasis = RandBasis.withSeed(2)
    var t2 = new Gaussian(0, 1).sample(10)
    var t3 = new Gaussian(0, 1).sample(10)

    assert { t2 != t3 } // sanity check

    val threads = for (i <- 1 to 2)
      yield
        new Thread {
          override def run(): Unit = { t2 = new Gaussian(0, 1).sample(10) }
        }
    threads.map(_.start)
    threads.map(_.join)

    // ensure that both threads use different seeds
    assert { t2 != t3 }
  }

  test("RandBasis.withSeed lets users specify a random seed") {
    {
      val a = RandBasis.withSeed(0).randInt.sample(100)
      val b = RandBasis.withSeed(0).randInt.sample(100)
      assert(a == b)
    }

    {
      val a = RandBasis.withSeed(0).randInt.sample(100)
      val b = RandBasis.withSeed(1).randInt.sample(100)
      assert(a != b)
    }
  }

  test("RandBasis.choose honors specified random seed") {
    val items = 'a' to 'z'

    {
      val a = RandBasis.withSeed(0).choose(items).sample(100)
      val b = RandBasis.withSeed(0).choose(items).sample(100)
      assert(a == b)
    }

    {
      val items = 'a' to 'z'
      val a = RandBasis.withSeed(0).choose(items).sample(100)
      val b = RandBasis.withSeed(1).choose(items).sample(100)
      assert(a != b)
    }
  }

}
