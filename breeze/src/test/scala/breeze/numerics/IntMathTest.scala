package breeze.numerics

/*
 Copyright 2012 David Hall

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
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class IntMathTest extends FunSuite {
  test("ipow") {
    import IntMath._
    assert(ipow(3, 4) === 81)
    assert(ipow(3, 1) === 3)
    assert(ipow(3, 0) === 1)
    assert(ipow(3, 3) === 27)
    assert(ipow(3, 12) === 531441)
  }

}
