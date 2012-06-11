package minla.linalg

import scala.{specialized=>spec}
import support._

/**
 * A Tensor defines a map from an index set to a set of values
 *
 * @author dlwh
 */
trait TensorLike[@spec(Int) K, @specialized V, +This<:Tensor[K, V]] extends NumericOps[This] {
  def apply(i: K):V

  def size: Int
  def activeSize: Int

  // iteration and such
  def keys: TensorKeys[K, V, This] = new TensorKeys[K, V, This](repr, false)
  def values: TensorValues[K, V, This] = new TensorValues[K, V, This](repr, false)
  def pairs: TensorPairs[K, V, This] = new TensorPairs[K, V, This](repr, false)
  def active: TensorActive[K, V, This] = new TensorActive[K, V, This](repr)

  def iterator: Iterator[(K, V)]
  def activeIterator: Iterator[(K, V)]

  def valuesIterator: Iterator[V]
  def activeValuesIterator: Iterator[V]

  def keysIterator: Iterator[K]
  def activeKeysIterator: Iterator[K]

  // slicing
  def apply[Slice, Result](slice: Slice)(implicit canSlice: CanSlice[This, Slice, Result]) = {
    canSlice(repr, slice)
  }
  def apply[Result](a: K, slice: K*)(implicit canSlice: CanSlice[This, Seq[K], Result]) = {
    canSlice(repr, a +: slice)
  }

  def apply[Slice1, Slice2, Result](slice1: Slice1, slice2: Slice2)(implicit canSlice: CanSlice2[This, Slice1, Slice2, Result]) = {
    canSlice(repr, slice1, slice2)
  }

  // Aggregators
  def max(implicit ord: Ordering[V]) = valuesIterator.max
  def min(implicit ord: Ordering[V]) = valuesIterator.min
  def argmax(implicit ord: Ordering[V]) = keysIterator.maxBy( apply _)
  def argmin(implicit ord: Ordering[V]) = keysIterator.minBy( apply _)
  def sum(implicit num: Numeric[V]) = valuesIterator.sum

  /** Creates a new map containing a transformed copy of this map. */
  def mapPairs[TT>:This,O,That](f : (K,V) => O)(implicit bf : CanMapKeyValuePairs[TT, K, V, O, That]) : That = {
    bf.map(repr, f)
  }

  /** Maps all active key-value pairs values. */
  def mapActivePairs[TT>:This,O,That](f : (K,V) => O)(implicit bf : CanMapKeyValuePairs[TT, K, V, O, That]) : That = {
    bf.mapActive(repr.asInstanceOf[TT], f)
  }

  /** Creates a new map containing a transformed copy of this map. */
  def mapValues[TT>:This,O,That](f : V => O)(implicit bf : CanMapValues[TT, V, O, That]) : That = {
    bf.map(repr.asInstanceOf[TT], f)
  }

  /** Maps all non-zero values. */
  def mapActiveValues[TT>:This,O,That](f : V => O)(implicit bf : CanMapValues[TT, V, O, That]) : That = {
    bf.mapActive(repr.asInstanceOf[TT], f)
  }
}

trait Tensor[@spec(Int) K, @specialized V] extends TensorLike[K, V, Tensor[K, V]]

