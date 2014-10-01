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

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class ImplicitsTest extends FunSuite {

  import Implicits._
  test("Set#toMultiMap") {
    assert(Set( (1, 2), (1, 3), (1, 2), (2,4)).toMultiMap  === Map(1 -> Set(2, 3), 2 -> Set(4)))
  }

  test("Seq#toMultiMap") {
    assert(Seq( (1, 2), (1, 3), (1, 2), (2,4)).toMultiMap  === Map(1 -> Seq(2, 3, 2), 2 -> Seq(4)))
  }

  test("IndexedSeq#toMultiMap") {
    assert(IndexedSeq( (1, 2), (1, 3), (1, 2), (2,4)).toMultiMap  === Map(1 -> IndexedSeq(2, 3, 2), 2 -> IndexedSeq(4)))
  }

}
