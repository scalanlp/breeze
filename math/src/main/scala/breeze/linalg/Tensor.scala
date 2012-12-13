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

import operators.CanTranspose
import scala.{specialized=>spec}
import support._
import breeze.generic.{UReduceable, URFunc, CanMapValues}
import collection.mutable
import breeze.collection.mutable.Beam


/**
 * We occasionally need a Tensor that doesn't extend NumericOps directly. This is that tensor.
 * @tparam K
 * @tparam V
 */
sealed trait QuasiTensor[@specialized(Int) K, @specialized(Int, Float, Double) V] {
  def apply(i: K):V
  def update(i: K, v: V)
  def keySet: scala.collection.Set[K]

    // Aggregators
  def max(implicit ord: Ordering[V]) = valuesIterator.max
  def min(implicit ord: Ordering[V]) = valuesIterator.min
  def argmax(implicit ord: Ordering[V]) = keysIterator.maxBy( apply _)
  def argmin(implicit ord: Ordering[V]) = keysIterator.minBy( apply _)
  def sum(implicit num: Numeric[V]) = valuesIterator.sum


  def argsort(implicit ord : Ordering[V]) : IndexedSeq[K] =
    keysIterator.toIndexedSeq.sorted(ord.on[K](apply _))

  /**
   * Returns the k indices with maximum value. (NOT absolute value.)
   * @param k how many to return
   * @param ordering
   * @return
   */
  def argtopk(k: Int)(implicit ordering: Ordering[V]) = {
    implicit val ordK = ordering.on(apply _)
    val queue = new Beam[K](k)
    queue ++= keysIterator
    queue.toIndexedSeq.reverse
  }

  def ureduce[A](f: URFunc[V, A]) = f(this.valuesIterator)

  def iterator: Iterator[(K, V)]
  def activeIterator: Iterator[(K, V)]

  def valuesIterator: Iterator[V]
  def activeValuesIterator: Iterator[V]

  def keysIterator: Iterator[K]
  def activeKeysIterator: Iterator[K]

  /** Returns all indices k whose value satisfies a predicate. */
  def findAll(f: V=>Boolean) = activeIterator.filter(p => f(p._2)).map(_._1).toIndexedSeq
}


/**
 * A Tensor defines a map from an index set to a set of values
 *
 * @author dlwh
 */
trait TensorLike[@spec(Int) K, @specialized(Int, Float, Double) V, +This<:Tensor[K, V]] extends QuasiTensor[K,V] with NumericOps[This] {
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



  /** Applies the given function to each key in the tensor. */
  def foreachKey[U](fn: K => U) : Unit =
    keysIterator.foreach[U](fn)

  /**
   * Applies the given function to each key and its corresponding value.
   */
  def foreachPair[U](fn: (K,V) => U) : Unit =
    foreachKey[U](k => fn(k,apply(k)))


  /**
   * Applies the given function to each value in the map (one for
   * each element of the domain, including zeros).
   */
  def foreachValue[U](fn : (V=>U)) =
    foreachKey[U](k => fn(apply(k)))


  /** Returns true if and only if the given predicate is true for all elements. */
  def forall(fn : (K,V) => Boolean) : Boolean = {
    foreachPair((k,v) => if (!fn(k,v)) return false)
    true
  }

  /** Returns true if and only if the given predicate is true for all elements. */
  def forallValues(fn : V => Boolean) : Boolean = {
    foreachValue(v => if (!fn(v)) return false)
    true
  }


}


trait Tensor[@spec(Int) K, @specialized(Int, Float, Double) V] extends TensorLike[K, V, Tensor[K, V]]

object Tensor {
  implicit def canUReduce[T, I, V](implicit ev: T<:<Tensor[I, V]):UReduceable[T, V] = {
    new UReduceable[T, V] {
      def apply[Final](c: T, f: URFunc[V, Final]): Final = c.ureduce(f)
    }
  }

  implicit def canSliceTensor[K, V:ClassManifest]:CanSlice[Tensor[K,V], Seq[K], SliceVector[K, V]] = new CanSlice[Tensor[K,V], Seq[K], SliceVector[K, V]] {
    def apply(from: Tensor[K, V], slice: Seq[K]): SliceVector[K, V] = new SliceVector(from, slice.toIndexedSeq)
  }

  implicit def canSliceTensor2[K1, K2, V:ClassManifest]:CanSlice2[Tensor[(K1,K2),V], Seq[K1], Seq[K2], SliceMatrix[K1, K2, V]] = {
    new CanSlice2[Tensor[(K1,K2),V], Seq[K1], Seq[K2], SliceMatrix[K1, K2, V]] {
      def apply(from: Tensor[(K1, K2), V], slice: Seq[K1], slice2: Seq[K2]): SliceMatrix[K1, K2, V] = {
        new SliceMatrix(from, slice.toIndexedSeq, slice2.toIndexedSeq)
      }
    }
  }
}

