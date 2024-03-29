/*
 *
 *  Copyright 2017 David Hall
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

package breeze.linalg


import org.scalatest.funsuite.AnyFunSuite

/**
 * TODO
 *
 * @author dlwh
  **/
class normalizeTest extends AnyFunSuite {

  test("#667 normalize float") {
    assert(normalize(DenseVector(3.0f, 4.0f)) == DenseVector(3.0f / 5.0f, 4.0f / 5.0f))
  }

}
