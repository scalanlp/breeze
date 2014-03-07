package breeze.numerics.financial

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

import breeze.linalg.support.CanTraverseValues
/**
 *
 * @author stucchio
 */
@RunWith(classOf[JUnitRunner])
class FinancialTest extends FunSuite {
  test("NetPresentValue") {
    assert(netPresentValue(1.0, Seq(1)) == 1.0)
    assert(netPresentValue(1.0, Seq(1,1)) == 1.5)
    assert(netPresentValue(1.0, Seq(1, 1, 1)) == 1.75)
    assert(netPresentValue(1.0, Seq(1, 1, 2)) == 2.0)
  }
  test("FutureValue") {
    assert(math.abs(futureValue(0.05/12, 10*12, -100, -100) - 15692.92889) < 1e-5)
    assert(futureValue(0.0, 3, 1, 1) == -4.0)
  }
  test("presentValue") {
    assert(math.abs(presentValue(0.05/12, 10*12, -100, 15692.93) - -100.0006713) < 1e-5)
    assert(presentValue(0, 3, 1, 1) == -4.0)
  }
}
