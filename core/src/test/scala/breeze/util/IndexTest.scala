/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

package breeze.util

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class IndexTest extends FunSuite with Checkers {

  test("CompositeIndex") {
    val index = Index(List("a","b","c","d"))
    val index2 = Index(List("e","f","g","h"))
    val comp = new CompositeIndex(index, index2)
    assert(comp(1 -> "e") === 4)
    assert(comp(0 -> "e") === -1)
  }

  test("PairIndex") {
    val index = Index(List("a","b","c","d"))
    val index2 = Index(List("e","f","g","h"))
    val comp = new PairIndex(index, index2)
    assert(comp("a" -> "e") === 0)
    assert(comp("e" -> "e") === -1)
  }


  test("EitherIndex") {
    val index = Index(List("a","b","c","d"))
    val index2 = Index(List("e","f","g","h"))
    val comp = new EitherIndex(index, index2)
    assert(comp(Right("e")) === 4)
    assert(comp(Left("e")) === -1)
  }

  test("EnumerationIndex") {
    object E extends Enumeration {
      val A, B, C, D = Value
    }
    val index:Index[E.Value] = EnumerationIndex(E)

    assert(index.get(0) === E.A)
    assert(index.get(1) === E.B)
    assert(index(E.A) === 0)
  }
}
