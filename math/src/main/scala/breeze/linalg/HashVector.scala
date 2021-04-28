package breeze.linalg

import operators._
import support._
import support.CanTraverseValues.ValuesVisitor
import support.CanTraverseKeyValuePairs.KeyValuePairsVisitor
import breeze.collection.mutable.OpenAddressHashArray
import breeze.storage.Zero
import breeze.macros.expand
import breeze.math._
import breeze.util.ReflectionUtil

import scala.{specialized => spec}
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

/**
 * A HashVector is a sparse vector backed by an OpenAddressHashArray
 * @author dlwh
 */
class HashVector[@spec(Double, Int, Float, Long) E](val array: OpenAddressHashArray[E])
    extends Vector[E]
    with VectorLike[E, HashVector[E]]
    with Serializable {

  // don't delete
  HashVector.init()

  def activeIterator: Iterator[(Int, E)] = array.activeIterator

  def activeValuesIterator: Iterator[E] = array.activeValuesIterator

  def activeKeysIterator: Iterator[Int] = array.activeKeysIterator

  def apply(i: Int): E = array(i)

  def update(i: Int, v: E): Unit = {
    array(i) = v
  }

  def default = array.defaultValue

  def activeSize: Int = array.activeSize

  def length: Int = array.length

  def copy: HashVector[E] = new HashVector(array.copy)

  def repr = this

  final def iterableSize: Int = array.iterableSize
  def data = array.data
  final def index = array.index
  final def isActive(i: Int) = array.isActive(i)

  def clear(): Unit = array.clear()

  override def toString = {
    activeIterator.mkString("HashVector(", ", ", ")")
  }

  def allVisitableIndicesActive: Boolean = false

  override def hashCode() = {
    var hash = 47
    // we make the hash code based on index * value, so that zeros don't affect the hashcode.
    val dv = array.default.value(array.zero)
    var i = 0
    while (i < activeSize) {
      if (isActive(i)) {
        val ind = index(i)
        val v = data(i)
        if (v != dv) {
          hash = MurmurHash3.mix(hash, v.##)
          hash = MurmurHash3.mix(hash, ind)
        }
      }

      i += 1
    }

    MurmurHash3.finalizeHash(hash, activeSize)
  }
}

object HashVector {
  def zeros[@spec(Double, Int, Float, Long) V: ClassTag: Zero](size: Int) = {
    new HashVector(new OpenAddressHashArray[V](size))
  }
  def apply[@spec(Double, Int, Float, Long) V: Zero](values: Array[V]) = {
    implicit val ctg: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(values)
    val oah = new OpenAddressHashArray[V](values.length)
    for ((v, i) <- values.zipWithIndex) oah(i) = v
    new HashVector(oah)
  }

  def apply[V: ClassTag: Zero](values: V*): HashVector[V] = {
    apply(values.toArray)
  }
  def fill[@spec(Double, Int, Float, Long) V: ClassTag: Zero](size: Int)(v: => V): HashVector[V] =
    apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float, Long) V: ClassTag: Zero](size: Int)(f: Int => V): HashVector[V] =
    apply(Array.tabulate(size)(f))

  def apply[V: ClassTag: Zero](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for ((i, v) <- values) {
      r(i) = v
    }
    r
  }

  // implicits

  implicit def canCreateZeros[V: ClassTag: Zero]: CanCreateZeros[HashVector[V], Int] =
    new CanCreateZeros[HashVector[V], Int] {
      def apply(d: Int): HashVector[V] = {
        zeros[V](d)
      }
    }

  // implicits
  class CanCopyHashVector[@specialized(Int, Float, Double) V: ClassTag: Zero] extends CanCopy[HashVector[V]] {
    def apply(v1: HashVector[V]) = {
      v1.copy
    }
  }

  implicit def canCopyHash[@specialized(Int, Float, Double) V: ClassTag: Zero]: CanCopyHashVector[V] =
    new CanCopyHashVector[V]

  implicit def canMapValues[V, V2: ClassTag: Zero]: CanMapValues[HashVector[V], V, V2, HashVector[V2]] = {
    new CanMapValues[HashVector[V], V, V2, HashVector[V2]] {
      def apply(from: HashVector[V], fn: (V) => V2) = {
        HashVector.tabulate(from.length)(i => fn(from(i)))
      }
    }
  }

  implicit def canMapActiveValues[V, V2: ClassTag: Zero]: CanMapActiveValues[HashVector[V], V, V2, HashVector[V2]] = {
    new CanMapActiveValues[HashVector[V], V, V2, HashVector[V2]] {
      def apply(from: HashVector[V], fn: (V) => V2) = {
        val out = new OpenAddressHashArray[V2](from.length)
        var i = 0
        while (i < from.iterableSize) {
          if (from.isActive(i))
            out(from.index(i)) = fn(from.data(i))
          i += 1
        }
        new HashVector(out)
      }
    }
  }

  implicit def scalarOf[T]: ScalarOf[HashVector[T], T] = ScalarOf.dummy

  implicit def canIterateValues[V]: CanTraverseValues[HashVector[V], V] = {
    new CanTraverseValues[HashVector[V], V] {

      def isTraversableAgain(from: HashVector[V]): Boolean = true

      def traverse(from: HashVector[V], fn: ValuesVisitor[V]): Unit = {
        fn.zeros(from.size - from.activeSize, from.default)
        var i = 0
        while (i < from.iterableSize) {
          if (from.isActive(i))
            fn.visit(from.data(i))
          i += 1
        }
      }
    }
  }

  implicit def canTraverseKeyValuePairs[V]: CanTraverseKeyValuePairs[HashVector[V], Int, V] = {
    new CanTraverseKeyValuePairs[HashVector[V], Int, V] {

      def traverse(from: HashVector[V], fn: KeyValuePairsVisitor[Int, V]): Unit = {
        fn.zeros(
          from.size - from.activeSize,
          Iterator.range(0, from.size).filterNot(from.index contains _),
          from.default)
        var i = 0
        while (i < from.iterableSize) {
          if (from.isActive(i))
            fn.visit(from.index(i), from.data(i))
          i += 1
        }
      }

      def isTraversableAgain(from: HashVector[V]): Boolean = true

      def traverse(from: HashVector[V], fn: ValuesVisitor[V]): Unit = {}
    }
  }

  implicit def canMapPairs[V, V2: ClassTag: Zero]: CanMapKeyValuePairs[HashVector[V], Int, V, V2, HashVector[V2]] = {
    new CanMapKeyValuePairs[HashVector[V], Int, V, V2, HashVector[V2]] {

      /**Maps all key-value pairs from the given collection. */
      def map(from: HashVector[V], fn: (Int, V) => V2) = {
        HashVector.tabulate(from.length)(i => fn(i, from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: HashVector[V], fn: (Int, V) => V2) = {
        val out = new OpenAddressHashArray[V2](from.length)
        var i = 0
        while (i < from.iterableSize) {
          if (from.isActive(i))
            out(from.index(i)) = fn(from.index(i), from.data(i))
          i += 1
        }
        new HashVector(out)
      }
    }
  }

  implicit def space[E: Field: ClassTag: Zero]: MutableFiniteCoordinateField[HashVector[E], Int, E] = {
    implicit val _dim: dim.Impl[HashVector[E], Int] = dim.implVDim[E, HashVector[E]]
    implicit val n: norm.Impl2[HashVector[E], Double, Double] = norm.canNorm(HasOps.impl_CanTraverseValues_HV_Generic, implicitly[Field[E]].normImpl)
    MutableFiniteCoordinateField.make[HashVector[E], Int, E]
  }

  @noinline
  private def init() = {}
}
