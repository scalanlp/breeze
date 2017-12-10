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

package breeze.collection.mutable

import org.scalatest.FunSuite

/**
  * TODO
  *
  * @author dlwh
  **/
class RingBufferTest extends FunSuite {

  test("#666 RingBuffer problems") {
    val rb = RingBuffer(3)(1,2,3)
    rb += 4

    assert(rb == RingBuffer(3)(2, 3, 4))

    val rb2 = RingBuffer(3)(0, 0, 0)
    rb2 += 1
    
    assert(rb2 == RingBuffer(3)(0, 0, 1))
  }

  test("RingBuffer delete front and back") {
    val rb = RingBuffer(3)(1,2,3)
    rb += 4
    rb += 5
    rb.remove(0)
    rb.remove(1)
    assert(rb == RingBuffer(3)(4))
  }

  test("RingBuffer delete back, push front") {
    val rb = RingBuffer(3)(1,2,3)
    rb += 4
    rb += 5
    rb.remove(2)
    rb.+=:(6)
    assert(rb == RingBuffer(3)(6, 3, 4))
  }

  test("RingBuffer delete front, push back") {
    val rb = RingBuffer(3)(1,2,3)
    rb += 4
    rb += 5
    rb.remove(0)
    rb += 6
    assert(rb == RingBuffer(3)(4, 5, 6))
  }

  test("RingBuffer push back, delete front") {
    val rb = RingBuffer(3)(1,2,3)
    rb += 4
    rb += 5
    rb += 6
    rb.remove(0)
    assert(rb == RingBuffer(3)(5, 6))
  }

  test("RingBuffer delete front") {
    val rb = RingBuffer(3)(1,2,3)
    rb += 4
    rb += 5
    rb.remove(0)
    assert(rb == RingBuffer(3)(4, 5))
  }

  test("RingBuffer delete mid 1") {
    val rb = RingBuffer(3)(1,2,3)
    rb += 4
    rb += 5
    rb.remove(1)
    assert(rb == RingBuffer(3)(3, 5))
  }

  test("RingBuffer delete mid 2") {
    val rb = RingBuffer(4)(1,2,3,4)
    rb += 5
    rb += 6
    rb.remove(1, 2)
    assert(rb == RingBuffer(4)(3, 6))
  }


  test("RingBuffer insertAll") {
    val rb = RingBuffer(4)(1,2,3,4)
    rb += 5
    rb += 6
    rb.insertAll(1, Seq(7, 8))
    assert(rb == RingBuffer(4)(8, 4, 5, 6))
  }

  

}
