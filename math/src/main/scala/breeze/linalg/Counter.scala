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

import breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support.CanTraverseKeyValuePairs.KeyValuePairsVisitor
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.math._
import breeze.storage.Zero

import scala.collection.Set

/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dramage, dlwh
 */
@SerialVersionUID(1)
trait CounterLike[K, V, +M <: scala.collection.mutable.Map[K, V], +This <: Counter[K, V]]
    extends TensorLike[K, V, This]
    with Serializable {
  def data: M
  def default: V

  def keySet: Set[K] = data.keySet

  def repr = this.asInstanceOf[This]

  override def size = data.size
  def activeSize = data.size

  def isEmpty = data.isEmpty

  def contains(k: K) = data.contains(k)

  override def apply(k: K) = {
    data.getOrElse(k, default)
  }

  def update(k: K, v: V): Unit = { data(k) = v }

  def get(k: K) = data.get(k)

  override def keysIterator = data.keysIterator

  override def valuesIterator = data.valuesIterator

  override def iterator = data.iterator

  def activeIterator = iterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = keysIterator

  override def toString: String = data.mkString("Counter(", ", ", ")")

  override def equals(p1: Any): Boolean = p1 match {
    case x: Counter[K, V] => x.data == this.data
    case _ => false
  }

  override def hashCode(): Int = data.hashCode()

  def toMap = data.toMap
}

trait Counter[K, V] extends Tensor[K, V] with CounterLike[K, V, collection.mutable.Map[K, V], Counter[K, V]] {}

object Counter extends CounterOps {

  /** Returns an empty counter. */
  def apply[K, V: Zero](): Counter[K, V] =
    new Impl(scala.collection.mutable.HashMap[K, V]())

  /** Returns a counter by summing all the given values. */
  def apply[K, V: Zero: Semiring](values: (K, V)*): Counter[K, V] =
    apply(values)

  /** Returns a counter by summing all the given values. */
  def apply[K, V: Zero: Semiring](values: TraversableOnce[(K, V)]): Counter[K, V] = {
    val rv = apply[K, V]()
    val field = implicitly[Semiring[V]]
    values.foreach({ case (k, v) => rv(k) = field.+(v, rv(k)) })
    rv
  }

  /** Counts each of the given items. */
  def countTraversable[K](items: TraversableOnce[K]): Counter[K, Int] = {
    val rv = apply[K, Int]()
    items.foreach(rv(_) += 1)
    rv
  }

  def count[K](items: K*): Counter[K, Int] = countTraversable(items)

  @SerialVersionUID(2872445575657408160L)
  class Impl[K, V](override val data: scala.collection.mutable.Map[K, V])(implicit zero: Zero[V])
      extends Counter[K, V] {
    def default = zero.zero
  }

  implicit def canMapValues[K, V, RV: Zero]: CanMapValues[Counter[K, V], V, RV, Counter[K, RV]] = {
    new CanMapValues[Counter[K, V], V, RV, Counter[K, RV]] {
      override def apply(from: Counter[K, V], fn: (V => RV)) = {
        val rv = Counter[K, RV]()
        for ((k, v) <- from.iterator) {
          rv(k) = fn(from.data(k))
        }
        rv
      }
    }
  }

  implicit def canMapActiveValues[K, V, RV: Zero]: CanMapActiveValues[Counter[K, V], V, RV, Counter[K, RV]] = {
    new CanMapActiveValues[Counter[K, V], V, RV, Counter[K, RV]] {
      override def apply(from: Counter[K, V], fn: (V => RV)) = {
        val rv = Counter[K, RV]()
        for ((k, v) <- from.activeIterator) {
          rv(k) = fn(from.data(k))
        }
        rv
      }
    }
  }

  implicit def canIterateValues[K, V]: CanTraverseValues[Counter[K, V], V] = new CanTraverseValues[Counter[K, V], V] {

    def isTraversableAgain(from: Counter[K, V]): Boolean = true

    /** Iterates all values from the given collection. */
    def traverse(from: Counter[K, V], fn: ValuesVisitor[V]): Unit = {
      for (v <- from.valuesIterator) {
        fn.visit(v)
      }
    }

  }

  implicit def scalarOf[K, V]: ScalarOf[Counter[K, V], V] = ScalarOf.dummy

  implicit def canTraverseKeyValuePairs[K, V]: CanTraverseKeyValuePairs[Counter[K, V], K, V] =
    new CanTraverseKeyValuePairs[Counter[K, V], K, V] {

      /** Traverses all values from the given collection. */
      override def traverse(from: Counter[K, V], fn: KeyValuePairsVisitor[K, V]): Unit = {
        for ((k, v) <- from.activeIterator) {
          fn.visit(k, v)
        }
      }

      override def isTraversableAgain(from: Counter[K, V]): Boolean = true
    }

  implicit def normImplDouble[K, V: Field]: norm.Impl2[Counter[K, V], Double, Double] =
    new norm.Impl2[Counter[K, V], Double, Double] {
      override def apply(ctr: Counter[K, V], p: Double): Double = {
        var result = 0.0
        for (v <- ctr.valuesIterator) {
          result += math.pow(implicitly[Field[V]].normImpl(v), p)
        }
        math.pow(result, 1 / p)
      }
    }

  implicit def canCreateZeros[K, V: Zero: Semiring]: CanCreateZeros[Counter[K, V], K] = {
    new CanCreateZeros[Counter[K, V], K] {
      // Shouldn't need to supply a key value here, but it really mixes up the
      // VectorSpace hierarchy since it would require separate types for
      // implicitly full-domain spaces (like Counter), and finite domain spaces, like Vector
      def apply(d: K): Counter[K, V] = {
        Counter()
      }
    }
  }

  implicit def canCreateZerosLike[K, V: Zero: Semiring]: CanCreateZerosLike[Counter[K, V], Counter[K, V]] = {
    new CanCreateZerosLike[Counter[K, V], Counter[K, V]] {
      // Shouldn't need to supply a key value here, but it really mixes up the
      // VectorSpace hierarchy since it would require separate types for
      // implicitly full-domain spaces (like Counter), and finite domain spaces, like Vector
      def apply(d: Counter[K, V]): Counter[K, V] = {
        val r = Counter[K, V]()
        val z = implicitly[Zero[V]].zero
        for ((k, v) <- d.iterator) {
          r(k) = z
        }
        r
      }
    }
  }

  implicit def space[K, V](implicit field: Field[V]): MutableEnumeratedCoordinateField[Counter[K, V], K, V] = {
    implicit def zipMap: Counter.CanZipMapValuesCounter[K, V, V] = Counter.zipMap[K, V, V]
    MutableEnumeratedCoordinateField.make[Counter[K, V], K, V]
  }
}
