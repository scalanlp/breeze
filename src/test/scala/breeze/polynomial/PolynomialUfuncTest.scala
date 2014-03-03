package breeze.polynomial

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
import breeze.linalg.DenseVector
import spire.math._
import spire.math.poly._
import spire.algebra._
import spire.implicits._

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class PolynomialUfuncTest extends FunSuite {

  test("PolyDenseUfuncWrapper applied to doubles") {
    val p = PolyDenseUFuncWrapper(Array[Double](1,2,4))
    assert(p[Double,Double](0.0) == 1.0)
    assert(p(1.0) == 7.0)
    assert(p(0.5) == 3.0)
  }
}
