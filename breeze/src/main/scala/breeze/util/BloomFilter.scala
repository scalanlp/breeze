package breeze.util

/*
 Copyright 2010 David Hall, Daniel Ramage

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


import scala.collection.mutable.BitSet

/**
 * A BloomFilter is an approximate set that sometimes gives false positives. That is,
 * if bf(x) returns true, then it might have been added to the set. If it returns false, then
 * it definitely has not. This is useful for caching and approximation.
 *
 * @author dlwh
 */
class BloomFilter[@specialized(Int, Long) T](val numBuckets: Int, val numHashFunctions: Int, val bits: BitSet) extends (T=>Boolean) {
  def this(numBuckets: Int, numHashFunctions: Int) = this(numBuckets, numHashFunctions, new BitSet(numBuckets));
  def this(numBuckets: Int) = this(numBuckets, 3);

  def apply(o: T): Boolean = {
    var h = 0;
    while(h < numHashFunctions) {
      val hash = computeHash(h,o);
      if(!bits.contains(hash%numBuckets)) return false;
      h+= 1
    }
    return true;
  }

  def contains(o: T) = apply(o);

  override def equals(other: Any) = other match {
    case that: BloomFilter[_] =>
      this.numBuckets == that.numBuckets && this.numHashFunctions == that.numHashFunctions && this.bits == that.bits
    case _ => false
  }

  def +=(o: T): this.type = {
    var h = 0;
    while(h < numHashFunctions) {
      val hash = computeHash(h,o);
      bits(hash%numBuckets) = true;
      h+= 1
    }
    this
  }

  def &(that: BloomFilter[T]) = {
    require(that.numBuckets == this.numBuckets,"Must have the same number of buckets to intersect");
    require(that.numHashFunctions == this.numHashFunctions, "Must have the same number of hash functions to intersect");
    new BloomFilter(this.numBuckets,this.numHashFunctions,this.bits & that.bits);
  }

  def |(that: BloomFilter[T]) = {
    require(that.numBuckets == this.numBuckets,"Must have the same number of buckets to compute union");
    require(that.numHashFunctions == this.numHashFunctions, "Must have the same number of hash functions to compute union");
    new BloomFilter(this.numBuckets,this.numHashFunctions,this.bits | that.bits);
  }

  def &~(that: BloomFilter[T]) = {
    require(that.numBuckets == this.numBuckets,"Must have the same number of buckets to intersect");
    require(that.numHashFunctions == this.numHashFunctions, "Must have the same number of hash functions to intersect");
    new BloomFilter(this.numBuckets,this.numHashFunctions,this.bits &~ that.bits);
  }


  private def computeHash(h: Int, o: T) = (h,o).hashCode.abs;

}


