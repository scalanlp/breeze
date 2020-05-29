package breeze.util

/*
 Copyright 2010 David Hall, Daniel Ramage

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

import java.util

/**
 * A BloomFilter is an approximate set that sometimes gives false positives. That is,
 * if bf(x) returns true, then it might have been added to the set. If it returns false, then
 * it definitely has not. This is useful for caching and approximation.
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class BloomFilter[@specialized(Int, Long) T](val numBuckets: Int, val numHashFunctions: Int, val bits: util.BitSet)
    extends (T => Boolean)
    with Serializable {
  def this(numBuckets: Int, numHashFunctions: Int) = this(numBuckets, numHashFunctions, new util.BitSet(numBuckets))
  def this(numBuckets: Int) = this(numBuckets, 3)

  private def activeBuckets(key: T) = {
    val baseHash = key.##
    // we only get 16 bits for each hash code, but we combine them in fun ways
    val hash1 = baseHash & 0xffff
    val hash2 = baseHash >> 16

    for {
      i <- 0 to numHashFunctions
    } yield {
      val h = (hash1 + i * hash2) % numBuckets
      if (h < 0) ~h else h
    }
  }

  def apply(o: T): Boolean = {
    activeBuckets(o).forall(i => bits.get(i))
  }

  def contains(o: T): Boolean = apply(o)

  /**
   *
   * Calculates the load of the bloom filter. If this is near 1, there will be lots of false positives.
   *
   * @return the fraction of bits that are set
   */
  def load: Double = bits.cardinality().toDouble / numBuckets

  override def hashCode(): Int = {
    this.bits.hashCode()
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: BloomFilter[_] =>
        this.numBuckets == that.numBuckets && this.numHashFunctions == that.numHashFunctions && this.bits == that.bits
      case _ => false
    }

  def +=(o: T): this.type = {
    activeBuckets(o).foreach(i => bits.set(i))
    this
  }

  def &(that: BloomFilter[T]): BloomFilter[T] = {
    checkCompatibility(that)
    new BloomFilter[T](this.numBuckets, this.numHashFunctions, this.bits & that.bits)
  }

  private def checkCompatibility(that: BloomFilter[T]): Unit = {
    require(that.numBuckets == this.numBuckets, "Must have the same number of buckets to intersect")
    require(that.numHashFunctions == this.numHashFunctions, "Must have the same number of hash functions to intersect")
  }

  def |(that: BloomFilter[T]): BloomFilter[T] = {
    checkCompatibility(that)
    new BloomFilter[T](this.numBuckets, this.numHashFunctions, this.bits | that.bits)
  }

  def |=(that: BloomFilter[T]): this.type = {
    checkCompatibility(that)
    this.bits |= that.bits
    this
  }

  def &=(that: BloomFilter[T]): this.type = {
    checkCompatibility(that)
    this.bits &= that.bits
    this
  }

  def &~=(that: BloomFilter[T]): this.type = {
    checkCompatibility(that)
    this.bits &~= that.bits
    this
  }

  def &~(that: BloomFilter[T]): BloomFilter[T] = {
    checkCompatibility(that)
    new BloomFilter[T](this.numBuckets, this.numHashFunctions, this.bits &~ that.bits)
  }

}

object BloomFilter {

  /**
   * Returns the optimal number of buckets  (m) and hash functions (k)
   *
   * The formula is:
   * {{{
   * val m = ceil(-(n * log(p)) / log(pow(2.0, log(2.0))))
   * val k = round(log(2.0) * m / n)
   * }}}
   *
   * @param expectedNumItems
   * @param falsePositiveRate
   * @return
   */
  def optimalSize(expectedNumItems: Double, falsePositiveRate: Double): (Int, Int) = {
    val n = expectedNumItems
    val p = falsePositiveRate
    import scala.math._
    val m = ceil(-(n * log(p)) / log(pow(2.0, log(2.0))))
    val k = round(log(2.0) * m / n)
    (m.toInt, k.toInt)
  }

  /**
   * Returns a BloomFilter that is optimally sized for the expected number of inputs and false positive rate
   * @param expectedNumItems
   * @param falsePositiveRate
   * @tparam T
   * @return
   */
  def optimallySized[T](expectedNumItems: Double, falsePositiveRate: Double): BloomFilter[T] = {
    val (buckets, funs) = optimalSize(expectedNumItems, falsePositiveRate)
    new BloomFilter(buckets, funs)
  }
}
