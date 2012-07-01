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

import TopKImplicits._

@RunWith(classOf[JUnitRunner])
class BloomFilterTest extends FunSuite with Checkers {
  test("add a bunch of strings and they're all there") {
    check { (strings: List[String], _numBuckets: Int, _numHashes: Int) => {
      val numHashes = _numHashes.abs % 1000 + 1
      val numBuckets = _numBuckets.abs % 1000 + 1
        val bf = new BloomFilter[String](numBuckets,numHashes.abs)
        strings foreach { bf += _ }
        strings forall { bf contains _ }
    }}
  }
}
