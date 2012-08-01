package breeze.linalg

/*
 Copyright 2012 Keith Stevens

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

import breeze.math.Ring

import scala.{specialized=>spec}

/**
 * This decorator automatically caches the norm and sums of a vector and invalidates the cache whenever the vector is updated.  It can be
 * applied to any {@code Vector} such as {@code DenseVector} or {@code SparseVector}.  Calls for the norm or sum after the initial request
 * will have a O(1) run time.  Calls to {@code update} will have a slight constant overhead used to invalidate the cache.  This wrapper is
 * especially beneficial for vectors that are rarely updated but frequently use the aggregate values, such as in cosine similarity.  
 *
 * </p>
 * Note that changing the backing datastructure of the {@code Vector} without using {@code update} will produce undefined behavior.
 *
 * @author fozziethebeat
 */
class NormCacheDecorator[@spec(Double, Int, Float) E](val vector: Vector[E]) extends Vector[E] {

  var sweep: Boolean = true
  var sumCache:Option[E] = None
  var normCache:Map[Double, Double] = Map[Double, Double]()

  /**
   * Invalidates the cache and then updates the value at index {@code i} with value {@code v}.
   */
  override def update(i: Int, v: E) {
    // Invalidate the caches.
    sweep = true
    sumCache = None
    normCache = Map[Double, Double]()
    // Call update as normal.
    vector.update(i, v)
  }

  /**
   * Returns a cached value (O(1) time) if the L2 norm is requested and a previous computation is still valid.  Otherwise, the norm is
   * computed (O(N) time) and the cached value is stored.
   */
  override def norm(n : Double)(implicit field: Ring[E]) : Double =
    // Check to see if the norm has been cached.  If so, return that value.  Otherwise, recompute the norm and cache it.
    normCache.get(n) match {
      case Some(cachedNorm) => cachedNorm
      case None => {
        sweep = false
        val computedNorm = vector.norm(n)
        // In a multi-threaded setting, update could be called while we are computing the norm.  If this happens, the sweep variable will be
        // set to true nothing that the norm value we've computed *may* be incorrect, so it should not be cached.
        if (!sweep)
            normCache += (n -> computedNorm)
        computedNorm
      }
    }

  override def sum(implicit num: Numeric[E]) =
    // Check to see if the norm has been cached.  If so, return that value.  Otherwise, recompute the norm and cache it.
    sumCache match {
      case Some(cachedSum) => cachedSum
      case None => {
        sweep = false
        val computedSum = vector.sum
        // In a multi-threaded setting, update could be called while we are computing the sum.  If this happens, the sweep variable will be
        // set to true nothing that the norm value we've computed *may* be incorrect, so it should not be cached.
        if (!sweep)
            sumCache = Some(computedSum)
        computedSum
      }
    }

  def activeIterator = vector.iterator

  def activeSize = vector.length

  def activeKeysIterator = vector.keysIterator

  def activeValuesIterator = vector.valuesIterator

  def apply(i: Int) = vector.apply(i)

  def copy = vector.copy

  def length = vector.length

  def repr = vector.repr
}
