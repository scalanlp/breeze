package breeze.linalg

import java.util

import breeze.linalg.operators.BitVectorOps
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.storage.Zero

import scala.reflect.ClassTag

/**
 * TODO
 *
 * @param enforceLength if false, then the BitVector won't throw exceptions if it's used in
 *                      operations with vectors longer than it.
 * @author dlwh
 * @author Martin Senne
 **/
class BitVector(val data: java.util.BitSet, val length: Int, val enforceLength: Boolean = true)
    extends Vector[Boolean]
    with VectorLike[Boolean, BitVector] {
  def apply(i: Int): Boolean = {
    if (i < 0 || (i >= length))
      throw new IndexOutOfBoundsException(s"$i is not in the range [0, $length)")
    data.get(i)
  }

  def update(i: Int, v: Boolean): Unit = {
    if (i < 0 || (i >= length))
      throw new IndexOutOfBoundsException(s"$i is not in the range [0, $length)")
    data.set(i, v)
  }

  def activeSize: Int = data.cardinality()

  def copy = new BitVector(data.clone().asInstanceOf[util.BitSet], length)

  def repr: BitVector = this

  def activeKeysIterator: Iterator[Int] = {
    val firstBit = data.nextSetBit(0)
    if (firstBit < 0) return Iterator.empty

    new Iterator[Int] {
      var nextReady = true
      var _next = firstBit
      def hasNext: Boolean =
        (_next >= 0) && (nextReady || {
          _next += 1
          _next = data.nextSetBit(_next)
          nextReady = _next >= 0
          nextReady
        })

      def next(): Int = {
        if (!nextReady) {
          hasNext
          if (!nextReady) throw new NoSuchElementException
        }
        nextReady = false
        _next
      }
    }

  }

  /** This will just be a bunch of true values. */
  def activeValuesIterator: Iterator[Boolean] = activeKeysIterator.map(_ => true)

  def activeIterator: Iterator[(Int, Boolean)] = activeKeysIterator.map(_ -> true)

  def lengthsMatch(other: Vector[_]): Boolean = {
    if (!enforceLength) true
    else
      other match {
        case x: BitVector => !x.enforceLength || x.length == length
        case _ => other.length == length
      }
  }

  override def toString = {
    activeKeysIterator.mkString("BitVector(", ", ", ")")
  }

}

object BitVector extends BitVectorOps {

  def apply(bools: Boolean*) = {
    val bs = new util.BitSet
    for (i <- 0 until bools.length if bools(i)) {
      bs.set(i)
    }

    new BitVector(bs, bools.length)
  }

  def apply(length: Int, enforceLength: Boolean = true)(trues: Int*) = {
    val bs = new util.BitSet
    for (i <- trues) {
      if (enforceLength && i >= length)
        throw new IndexOutOfBoundsException(s"$i is bigger than $length")
      bs.set(i)
    }
    new BitVector(bs, length, enforceLength && length >= 0)
  }

  def zeros(length: Int, enforceLength: Boolean = true): BitVector =
    new BitVector(new util.BitSet(), length, enforceLength)

  def ones(length: Int, enforceLength: Boolean = true) = {
    val bs = new java.util.BitSet(length)
    bs.set(0, length)
    new BitVector(bs, length, enforceLength)
  }

  implicit object traverseBitVector extends CanTraverseValues[BitVector, Boolean] {

    /** Traverses all values from the given collection. */
    def traverse(from: BitVector, fn: ValuesVisitor[Boolean]): Unit = {
      for (i <- from.valuesIterator) fn.visit(i)
    }

    def isTraversableAgain(from: BitVector): Boolean = true
  }

  implicit def canMapValues[V2](implicit man: ClassTag[V2]): CanMapValues[BitVector, Boolean, V2, DenseVector[V2]] = {
    new CanMapValues[BitVector, Boolean, V2, DenseVector[V2]] {

      /**Maps all key-value pairs from the given collection. */
      def apply(from: BitVector, fn: (Boolean) => V2): DenseVector[V2] = {
        DenseVector.tabulate(from.length)(i => fn(from(i)))
      }
    }
  }

  implicit def scalarOf[T]: ScalarOf[DenseMatrix[T], T] = ScalarOf.dummy

  implicit def canIterateValues: CanTraverseValues[BitVector, Boolean] =
    new CanTraverseValues[BitVector, Boolean] {

      def isTraversableAgain(from: BitVector): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: BitVector, fn: ValuesVisitor[Boolean]): Unit = {
        for (i <- 0 until from.length) {
          fn.visit(from(i))
        }
      }

    }

  implicit def canTraverseKeyValuePairs: CanTraverseKeyValuePairs[BitVector, Int, Boolean] =
    new CanTraverseKeyValuePairs[BitVector, Int, Boolean] {
      def isTraversableAgain(from: BitVector): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: BitVector, fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[Int, Boolean]): Unit = {
        for (i <- 0 until from.length) {
          fn.visit(i, from(i))
        }
      }

    }

  implicit def canTransformValues: CanTransformValues[BitVector, Boolean] =
    new CanTransformValues[BitVector, Boolean] {
      def transform(from: BitVector, fn: (Boolean) => Boolean): Unit = {
        for (i <- 0 until from.length) {
          from(i) = fn(from(i))
        }
      }

      def transformActive(from: BitVector, fn: (Boolean) => Boolean): Unit = {
        transform(from, fn)
      }
    }

  implicit def canMapPairs[V2](
      implicit man: ClassTag[V2],
      zero: Zero[V2]): CanMapKeyValuePairs[BitVector, Int, Boolean, V2, DenseVector[V2]] =
    new CanMapKeyValuePairs[BitVector, Int, Boolean, V2, DenseVector[V2]] {
      def map(from: BitVector, fn: (Int, Boolean) => V2): DenseVector[V2] = {
        DenseVector.tabulate(from.length)(i => fn(i, from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: BitVector, fn: (Int, Boolean) => V2): DenseVector[V2] = {
        val result = DenseVector.zeros[V2](from.length)
        for (i <- from.activeKeysIterator) {
          result(i) = fn(i, true)
        }
        result
      }
    }

}
