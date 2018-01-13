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
      val numBuckets = (_numBuckets/1000).abs % 1000 + 1
      assert(numBuckets >= 0, numBuckets + " " +  _numBuckets)
        val bf = new BloomFilter[String](numBuckets,numHashes.abs)
        strings foreach { bf += _ }
        strings forall { bf contains _ }
    }}
  }

  test("add single value and it's there") {
    check { (value:Long, _numBuckets: Int, _numHashes: Int) => 
      val numHashes = _numHashes.abs % 1000 + 1
      val numBuckets = (_numBuckets/1000).abs % 1000 + 1
      
      assert(numBuckets >= 0, numBuckets + " " +  _numBuckets)

      val bloomFilter = new BloomFilter[Long](numBuckets,numHashes.abs)
      bloomFilter += value

      bloomFilter.contains(value)
    }
  }

  test("union with empty is a copy") {
    check { (strings: List[String], _numBuckets: Int, _numHashes: Int) => {
      val numHashes = _numHashes.abs % 1000 + 1
      val numBuckets = (_numBuckets/1000).abs % 1000 + 1
      assert(numBuckets >= 0, numBuckets + " " +  _numBuckets)
      val bf = new BloomFilter[String](numBuckets,numHashes.abs)
      val bf2 = new BloomFilter[String](numBuckets,numHashes.abs)
      strings foreach { bf += _ }
      bf2 |=  bf
      strings forall { bf2 contains _ }
    }}
  }


  test("bloom filter mostly returns false for missing things") {
    check { (strings: Set[String], strings2: Set[String], _numBuckets: Int, _numHashes: Int) => {
      val numHashes =  5
      val numBuckets = 1000
      assert(numBuckets >= 0, numBuckets + " " +  _numBuckets)
      (
        attempt1(numBuckets, numHashes, strings, strings2)
        || attempt1(numBuckets + 1, numHashes + 1, strings, strings2)
        )
    }}
  }



  test("bloom filter works with objects with Int.MinValue hash") {
    val bloomFilter = BloomFilter.optimallySized[Int](100, 0.003)
    val value = Int.MinValue
    bloomFilter += value
    assert(bloomFilter.contains(value))
  }



  def attempt1(numBuckets: Int, numHashes: Int, strings: Set[String], strings2: Set[String]): Boolean = {
    val bf = new BloomFilter[String](numBuckets, numHashes.abs)
    val bf2 = new BloomFilter[String](numBuckets, numHashes.abs)
    strings foreach {
      bf += _
    }
    bf2 |= bf
    val numBad = (strings2 -- strings).count {
      bf2 contains _
    }
    numBad <= ((strings2 -- strings).size / 100) + 5
  }
}
