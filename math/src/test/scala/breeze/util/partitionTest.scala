/*
 *
 *  Copyright 2014 David Hall
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

package breeze.util

import org.scalatest.funsuite.AnyFunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class partitionTest extends AnyFunSuite {

  test("partition array in place") {
    val a = Array(3, 4, 2, 1)
    partition.inPlace(a, 3)
    assert(a(3) === 4)
  }

  test("partition array copy") {
    val a = Array(3, 4, 2, 1)
    val b = partition(a, 3)
    assert(b(3) === 4)
    assert(a.toIndexedSeq == IndexedSeq(3, 4, 2, 1))

  }

}
