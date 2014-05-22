package breeze.linalg

import java.util
import operators.BitVectorOps
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

/**
 * TODO
 *
 * @param enforceLength if false, then the BitVector won't throw exceptions if it's used in
 *                      operations with vectors longer than it.
 * @author dlwh
 * @author Martin Senne
 **/
class BitVector(val data: java.util.BitSet, val length: Int, val enforceLength: Boolean = true) extends Vector[Boolean] with VectorLike[Boolean, BitVector] {
  def apply(i: Int): Boolean = {
    if(i < 0 || (i >= length))
      throw new IndexOutOfBoundsException(s"$i is not in the range [0, $length)")
    data.get(i)
  }

  def update(i: Int, v: Boolean) {
    if(i < 0 || (i >= length))
      throw new IndexOutOfBoundsException(s"$i is not in the range [0, $length)")
    data.set(i, v)
  }

  def activeSize: Int = data.cardinality()

  def copy = new BitVector(data, length)

  def repr: BitVector = this

  def activeKeysIterator: Iterator[Int] = {
    val firstBit = data.nextSetBit(0)
    if(firstBit < 0) return Iterator.empty

    new Iterator[Int] {
      var nextReady = true
      var _next = firstBit
      def hasNext: Boolean = (_next >= 0) && (nextReady || {
        _next += 1
        _next = data.nextSetBit(_next)
        nextReady = _next >= 0
        nextReady
      })

      def next(): Int = {
        if(!nextReady) {
          hasNext
          if(!nextReady) throw new NoSuchElementException
        }
        nextReady = false
        _next
      }
    }



  }

  /** This will just be a bunch of true values. */
  def activeValuesIterator: Iterator[Boolean] = activeKeysIterator.map(_ => true)

  def activeIterator: Iterator[(Int, Boolean)] = activeKeysIterator.map(_ -> true)

  def lengthsMatch(other: Vector[_]) = {
    if(!enforceLength) true
    else other match {
      case x: BitVector => !x.enforceLength || x.length == length
      case _ => other.length == length
    }
  }

  override def toString = {
    activeKeysIterator.mkString("BitVector(",", ", ")")
  }

}

object BitVector extends BitVectorOps {

  def apply(bools: Boolean*) = {
    val bs = new util.BitSet
    for(i <- 0 until bools.length if bools(i)) {
      bs.set(i)
    }

    new BitVector(bs, bools.length)
  }

  def apply(length: Int, enforceLength: Boolean = true)(trues: Int*) = {
    val bs = new util.BitSet
    for(i <- trues) {
      if(enforceLength && i >= length)
        throw new IndexOutOfBoundsException(s"$i is bigger than $length")
      bs.set(i)
    }
    new BitVector(bs, length, enforceLength && length >= 0)
  }

  def zeros(length: Int, enforceLength: Boolean = true):BitVector = new BitVector(new util.BitSet(), length, enforceLength)

  def ones(length: Int, enforceLength: Boolean = true) = {
    val bs = new java.util.BitSet(length)
    bs.set(0, length)
    new BitVector(bs, length, enforceLength)
  }

  implicit object traverseBitVector extends CanTraverseValues[BitVector, Boolean] {
    /** Traverses all values from the given collection. */
    def traverse(from: BitVector, fn: ValuesVisitor[Boolean]): Unit = {
      for(i <- from.valuesIterator) fn.visit(i)
    }

    def isTraversableAgain(from: BitVector): Boolean = true
  }

}

