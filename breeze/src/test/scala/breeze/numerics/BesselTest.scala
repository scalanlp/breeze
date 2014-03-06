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
class BesselTest extends FunSuite {
  import Bessel._
  test("i0") {
    assert((i0(1) - 1.2660658777520083).abs < 1E-8)
    assert((i0(0) - 1.0).abs < 1E-8)
    assert((i0(20) - 4.355828255955353E7).abs < 1E-1)

  }

  test("i1") {
    assert((i1(1) - 0.565159103992485).abs < 1E-8, i1(1))
    assert((i1(0) - 0).abs < 1E-8)
    assert((i1(20) - 4.24549733851277E7).abs < 1E-1)

  }

}
