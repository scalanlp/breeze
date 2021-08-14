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
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._

class TopKTest extends AnyFunSuite with Checkers {

  test("Check that we always get the top elements: doubles") {
    check { (values: List[Double], k: Int) =>
      (k < 0) || (k > 30) || {
        val topk = TopK(k, values.iterator)
        topk.size == math.min(k, values.distinct.size) &&
        topk.iterator.zip(topk.iterator.drop(1)).forall(tup => (tup._1 >= tup._2)) &&
        (topk.isEmpty || {
          val set = topk.toSet
          val last = topk.min
          values.forall(n => n < last || set.contains(n))
        })
      }
    }
  }

  test("Check that we always get the top elements: ints") {
    check { (values: List[Int], k: Int) =>
      (k < 0) || (k > 30) || {
        val topk = TopK(k, values.iterator)
        topk.size == math.min(k, values.distinct.size) &&
        topk.iterator.zip(topk.iterator.drop(1)).forall(tup => (tup._1 >= tup._2)) &&
        (topk.isEmpty || {
          val set = topk.toSet
          val last = topk.min
          values.forall(n => n < last || set.contains(n))
        })
      }
    }
  }
}
