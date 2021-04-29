package breeze.linalg
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

import support._
import breeze.collection.mutable.Beam
import breeze.generic.UFunc
import breeze.math.Semiring
import breeze.linalg.operators.{HasOps, TensorLowPrio}

import scala.annotation.unchecked.uncheckedVariance
import scala.util.hashing.MurmurHash3
import scala.{specialized => spec}
import scala.reflect.ClassTag

/**
 * We occasionally need a Tensor that doesn't extend NumericOps directly. This is that tensor.
 * @tparam K
 * @tparam V
 */
trait QuasiTensor[@spec(Int) K, @spec(Double, Int, Float, Long) V] extends HasOps {
  def apply(i: K): V
  def update(i: K, v: V): Unit
  def keySet: scala.collection.Set[K]

  // Aggregators
  @deprecated("Use max(t) instead of t.max", "0.6")
  def max(implicit ord: Ordering[V]) = valuesIterator.max
  @deprecated("Use min(t) instead of t.min", "0.6")
  def min(implicit ord: Ordering[V]) = valuesIterator.min
  @deprecated("Use argmax(t) instead of t.argmax", "0.6")
  def argmax(implicit ord: Ordering[V]) = keysIterator.maxBy(apply _)
  @deprecated("Use argmin(t) instead of t.argmin", "0.6")
  def argmin(implicit ord: Ordering[V]) = keysIterator.minBy(apply _)
  @deprecated("Use sum(t) instead of t.sum", "0.6")
  def sum(implicit num: Numeric[V]) = activeValuesIterator.sum
  @deprecated("Use argsort(t) instead of t.argsort", "0.6")
  def argsort(implicit ord: Ordering[V]): IndexedSeq[K] =
    keysIterator.toIndexedSeq.sorted(ord.on[K](apply _))

  /**
   * Returns the k indices with maximum value. (NOT absolute value.)
   * @param k how many to return
   * @param ordering
   * @return
   */
  @deprecated("Use argtopk(t, k) instead of t.argtopk(k)", "0.6")
  def argtopk(k: Int)(implicit ordering: Ordering[V]) = {
    implicit val ordK: Ordering[K] = ordering.on(apply _)
    val queue = new Beam[K](k)
    queue ++= keysIterator
    queue.toIndexedSeq.reverse
  }

  def iterator: Iterator[(K, V)]
  def activeIterator: Iterator[(K, V)]

  def valuesIterator: Iterator[V]
  def activeValuesIterator: Iterator[V]

  def keysIterator: Iterator[K]
  def activeKeysIterator: Iterator[K]

  /** Returns all indices k whose value satisfies a predicate. */
  def findAll(f: V => Boolean) = activeIterator.filter(p => f(p._2)).map(_._1).toIndexedSeq

  /** Returns true if all elements are non-zero */
  @deprecated("Use breeze.linalg.all instead", "0.6")
  def all(implicit semi: Semiring[V]) = valuesIterator.forall(_ != semi.zero)

  /** Returns true if some element is non-zero */
  @deprecated("Use breeze.linalg.any instead", "0.6")
  def any(implicit semi: Semiring[V]) = valuesIterator.exists(_ != semi.zero)

  override def hashCode() = {
    var hash = 43
    for (v <- activeValuesIterator) {
      val hh = v.##
      if (hh != 0)
        hash = MurmurHash3.mix(hash, hh)
    }

    hash
  }
}

trait TensorLike[@spec(Int) K, @spec(Double, Int, Float, Long) V, +This <: Tensor[K, V]]
    extends QuasiTensor[K, V]
    with NumericOps[This] {

  def apply(i: K): V
  def update(i: K, v: V): Unit

  def size: Int
  def activeSize: Int

  // iteration and such
  def keys: TensorKeys[K, V, This] = new TensorKeys[K, V, This](repr, false)
  def values: TensorValues[K, V, This] = new TensorValues[K, V, This](repr, false)
  def pairs: TensorPairs[K, V, This] = new TensorPairs[K, V, This](repr, false)
  def active: TensorActive[K, V, This] = new TensorActive[K, V, This](repr)

  // slicing
  /**
   * method for slicing a tensor. For instance, DenseVectors support efficient slicing by a Range object.
   * @return
   */
  def apply[Slice, Result](slice: Slice)(implicit canSlice: CanSlice[This, Slice, Result]) = {
    canSlice(repr, slice)
  }

  /**
   * Slice a sequence of elements. Must be at least 2.
   * @param a
   * @param slice
   * @param canSlice
   * @tparam Result
   * @return
   */
  def apply[Result](a: K, b: K, c: K, slice: K*)(implicit canSlice: CanSlice[This, Seq[K], Result]) = {
    canSlice(repr, a +: b +: c +: slice)
  }

  /**
   * Method for slicing that is tuned for Matrices.
   * @return
   */
  def apply[Slice1, Slice2, Result](slice1: Slice1, slice2: Slice2)(
      implicit canSlice: CanSlice2[This, Slice1, Slice2, Result]) = {
    canSlice(repr, slice1, slice2)
  }

  /** Creates a new map containing a transformed copy of this map. */
  def mapPairs[O, That](f: (K, V) => O)(implicit bf: CanMapKeyValuePairs[This, K, V, O, That]): That = {
    bf.map(repr, f)
  }

  /** Maps all active key-value pairs values. */
  def mapActivePairs[O, That](f: (K, V) => O)(implicit bf: CanMapKeyValuePairs[This, K, V, O, That]): That = {
    bf.mapActive(repr, f)
  }

  /** Creates a new map containing a transformed copy of this map. */
  def mapValues[O, That](f: V => O)(implicit bf: CanMapValues[This @uncheckedVariance, V, O, That]): That = {
    bf(repr, f)
  }

  /** Maps all non-zero values. */
  def mapActiveValues[O, That](f: V => O)(implicit bf: CanMapActiveValues[This @uncheckedVariance, V, O, That]): That = {
    bf(repr, f)
  }

  /** Applies the given function to each key in the tensor. */
  def foreachKey[U](fn: K => U): Unit =
    keysIterator.foreach[U](fn)

  /**
   * Applies the given function to each key and its corresponding value.
   */
  def foreachPair[U](fn: (K, V) => U): Unit =
    foreachKey[U](k => fn(k, apply(k)))

  /**
   * Applies the given function to each value in the map (one for
   * each element of the domain, including zeros).
   */
  def foreachValue[U](fn: (V => U)) =
    foreachKey[U](k => fn(apply(k)))

  /** Returns true if and only if the given predicate is true for all elements. */
  def forall(fn: (K, V) => Boolean): Boolean = {
    foreachPair((k, v) => if (!fn(k, v)) return false)
    true
  }

  /** Returns true if and only if the given predicate is true for all elements. */
  @deprecated(
    "Please use 'forall' with the same arguments, which is more in accordance with scala.collections syntax",
    "0.8")
  def forallValues(fn: V => Boolean): Boolean = forall(fn)

  /** Returns true if and only if the given predicate is true for all elements. */
  def forall(fn: V => Boolean): Boolean = {
    foreachValue(v => if (!fn(v)) return false)
    true
  }

}

/**
 * A Tensor defines a map from an index set to a set of values.
 *
 * @author dlwh
 */
trait Tensor[@spec(Int) K, @spec(Double, Int, Float, Long) V] extends TensorLike[K, V, Tensor[K, V]]

object Tensor {

}


