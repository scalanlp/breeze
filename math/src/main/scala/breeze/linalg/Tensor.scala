package breeze.linalg

import scala.{specialized=>spec}
import support._
import breeze.generic.{UReduceable, URFunc, CanMapValues}


/**
 * We occasionally need a Tensor that doesn't extend NumericOps directly. This is that tensor.
 * @tparam K
 * @tparam V
 */
sealed trait QuasiTensor[@specialized(Int) K, @specialized V] {
  def apply(i: K):V
  def update(i: K, v: V)
  def keySet: scala.collection.Set[K]

    // Aggregators
  def max(implicit ord: Ordering[V]) = valuesIterator.max
  def min(implicit ord: Ordering[V]) = valuesIterator.min
  def argmax(implicit ord: Ordering[V]) = keysIterator.maxBy( apply _)
  def argmin(implicit ord: Ordering[V]) = keysIterator.minBy( apply _)
  def sum(implicit num: Numeric[V]) = valuesIterator.sum

  def ureduce[A](f: URFunc[V, A]) = f(this.valuesIterator)

  def iterator: Iterator[(K, V)]
  def activeIterator: Iterator[(K, V)]

  def valuesIterator: Iterator[V]
  def activeValuesIterator: Iterator[V]

  def keysIterator: Iterator[K]
  def activeKeysIterator: Iterator[K]
}

/**
 * A Tensor defines a map from an index set to a set of values
 *
 * @author dlwh
 */
trait TensorLike[@spec(Int) K, @specialized V, +This<:Tensor[K, V]] extends QuasiTensor[K,V] with NumericOps[This] {
  def apply(i: K):V
  def update(i: K, v: V)

  def size: Int
  def activeSize: Int

  // iteration and such
  def keys: TensorKeys[K, V, This] = new TensorKeys[K, V, This](repr, false)
  def values: TensorValues[K, V, This] = new TensorValues[K, V, This](repr, false)
  def pairs: TensorPairs[K, V, This] = new TensorPairs[K, V, This](repr, false)
  def active: TensorActive[K, V, This] = new TensorActive[K, V, This](repr)


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

object Tensor {
  implicit def canUReduce[T, I, V](implicit ev: T<:<Tensor[I, V]):UReduceable[T, V] = {
    new UReduceable[T, V] {
      def apply[Final](c: T, f: URFunc[V, Final]): Final = c.ureduce(f)
    }
  }
}

