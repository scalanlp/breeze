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

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class RandTest extends FunSuite {
  test("randInt is always non-negative") {
    assert( RandBasis.mt0.randInt.sample(10000).forall(_ >= 0) )
    assert( RandBasis.mt0.randLong.sample(10000).forall(_ >= 0) )
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

}
