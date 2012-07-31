package breeze.linalg

import breeze.math.Ring

import scala.{specialized=>spec}

/**
 * This trait automatically caches the L2 norm of a vector and invalidates the cache whenever the vector is updated.  It can be applied to
 * any {@code Vector} such as {@code DenseVector} or {@code SparseVector}.  Calls for the L2-norm after the initial call to {@code norm}
 * will have a O(1) run time.  Calls to {@code update} will have a slight constant overhead used to invalidate the cache.  This trait is
 * especially beneficial for vectors that are rarely updated but frequently use the norm, such as in cosine similarity.  
 *
 * </p>
 * Note that changing the backing datastructure of the {@code Vector} without using {@code update} will produce undefined behavior.
 *
 * </p>
 * This trait will be extended with additional functionality to cache other norms in the future.  
 *
 * @author fozziethebeat
 */
trait NormCacheVector[E] extends Vector[E] {

  /**
   * A cached value for the L2 Norm.  When the cache is invalidated, it will be set to -1, an impossible value for any norm.
   */
  var norm2 = -1d

  /**
   * Invalidates the cache and then updates the value at index {@code i} with value {@code v}.
   */
  abstract override def update(i: Int, v: E) {
    // Invalidate the norm cache and then update the index.
    norm2 = -1d
    super.update(i, v)
  }

  /**
   * Returns a cached value (O(1) time) if the L2 norm is requested and a previous computation is still valid.  Otherwise, the norm is
   * computed (O(N) time) and the cached value is stored.
   */
  abstract override def norm(n : Double)(implicit field: Ring[E]) : Double = {
    // If requesting the L2 norm and it is still valid, return the norm.
    if (n == 2 && norm2 != -1d)
      norm2
    else {
      // Otherwise re-compute the norm and cache the value (if it's L2).
      val v = super.norm(n)
      if (n == 2)
        norm2 = v
      v
    }
  }
}
