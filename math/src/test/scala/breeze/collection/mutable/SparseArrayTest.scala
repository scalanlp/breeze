package breeze.collection.mutable

/*
 Copyright 2012 David Hall

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

import breeze.linalg.SparseVector
import org.scalatest._
import org.scalatest.funsuite._
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._

class SparseArrayTest extends AnyFunSuite with Checkers {
  test("Map") {
    val x = SparseArray(1, 0, 2, 0, 3, 0, -1, -2, -3)
    x.compact()
    val y = x.map(_ + 1)
    assert(x.length === y.length)
    assert(x.activeSize === x.length - 3)
    assert(y.activeSize === 6, y)
    assert(y.toList === List(2, 1, 3, 1, 4, 1, 0, -1, -2))
  }

  test("Filter") {
    val x = SparseArray(1, 0, 2, 0, 3, 0, -1, -2, -3)
    x.compact()
    assert(x.filter(_ % 2 == 1).toList === List(1, 3))
    assert(x.filter(_ % 2 == 1).activeSize === 2)
    assert(x.filter(_ % 2 == 0).toList === List(0, 2, 0, 0, -2))
    assert(x.filter(_ % 2 == 0).activeSize === 2, x.filter(_ % 2 == 0))
    assert(x.filter(_ > 0).toList === List(1, 2, 3))
    assert(x.filter(_ > 0).activeSize === 3)
    assert(x.filter(_ >= 0).toList === List(1, 0, 2, 0, 3, 0))
    assert(x.filter(_ >= 0).activeSize === 3)

    val y = SparseArray(0, 1, 0, 0, -1, -2, -3, -5)
    y.compact()
    assert(y.filter(_ > 0).toList === List(1))
    assert(y.filter(_ >= 0).toList === List(0, 1, 0, 0))
  }

  test("equals") {
    val x = SparseArray.create(1024)(42 -> 100500, 24 -> 500100)
    assert(x === x)
    assert(x === SparseArray.create(1024)(42 -> 100500, 24 -> 500100))

    assert(x !== OpenAddressHashArray[Int](x.toArray: _*))
    assert(x !== SparseArray.create(128)(42 -> 100500, 24 -> 500100))
    assert(x !== SparseArray.create(1024)(42 -> 42, 24 -> 24))
  }

  test("equals is O(activeSize)") {
    val v = SparseVector(Int.MaxValue)(42 -> 100500)
    val u = SparseVector(Int.MaxValue)(42 -> 100500)
    assert(u === v)
  }

  test("#674 filter") {
    val arr = SparseArray(1, 2, 3)
    assert(arr.filter(_ => true) == arr)
    val arr2 = SparseArray(1, 0, 3, 0).filter(a => a != 3)
    assert(arr2 == SparseArray(1, 0, 0))
    assert(arr2.activeSize == 1)
  }
}
