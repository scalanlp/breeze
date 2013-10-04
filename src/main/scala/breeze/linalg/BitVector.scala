package breeze.linalg

import java.util
import breeze.macros.expand
import scala.math.BigInt
import breeze.linalg.operators._

/**
 * TODO
 *
 * @author dlwh
 **/
class BitVector(val data: java.util.BitSet, val length: Int, val enforceLength: Boolean = true) extends Vector[Boolean] with VectorLike[Boolean, BitVector] {
  def apply(i: Int): Boolean = {
    if(i < 0 || (i >= length))
      throw new IndexOutOfBoundsException(s"$i is not in the range [0, $length)")
    data.get(i)
  }

  def update(i: Int, v: Boolean) {
    if(i < 0 || (i >= length))
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
    val data = new Array[Long]( (length + 63)/64)
    util.Arrays.fill(data, -1L)
    val bs = util.BitSet.valueOf(data)
    bs.clear(length,data.length * 64)
    new BitVector(bs, length, enforceLength)
  }
}

trait BitVectorOps {

  @expand
  @expand.valify
  implicit def bv_bv_UpdateOp[@expand.args(OpAnd, OpOr, OpXor, OpSet) Op <: OpType]
  (implicit @expand.sequence[Op]({_ and _},  {_ or _}, {_ xor _}, { (a,b) => a.clear(); a.or(b)})
  op: BinaryUpdateOp[java.util.BitSet, java.util.BitSet, Op]):BinaryUpdateOp[BitVector, BitVector, Op] = new BinaryUpdateOp[BitVector, BitVector, Op] {
    def apply(a: BitVector, b: BitVector) {
      if(!a.lengthsMatch(b)) throw new IllegalArgumentException(s"Lengths don't match: ${a.length} ${b.length}")
      op(a.data, b.data)
    }
  }

  @expand
  @expand.valify
  implicit def bv_bv_Op[@expand.args(OpAnd, OpOr, OpXor) Op <: OpType]
  (implicit @expand.sequence[Op]({_ and _},  {_ or _}, {_ xor _})
  op: BinaryUpdateOp[java.util.BitSet, java.util.BitSet, Op]):BinaryOp[BitVector, BitVector, Op, BitVector] = new BinaryOp[BitVector, BitVector, Op, BitVector] {
    def apply(a: BitVector, b: BitVector) = {
      if(!a.lengthsMatch(b)) throw new IllegalArgumentException(s"Lengths don't match: ${a.length} ${b.length}")
      val result = a.data.clone().asInstanceOf[util.BitSet]
      op(result, b.data)
      new BitVector(result, a.length max b.length, a.enforceLength && b.enforceLength)
    }
  }


  implicit val bv_OpNot:UnaryOp[BitVector, OpNot, BitVector] = new UnaryOp[BitVector, OpNot, BitVector] {
    def apply(a: BitVector): BitVector = {
      val ones = BitVector.ones(a.length, a.enforceLength)
      ones.data.andNot(a.data)
      ones
    }
  }


  implicit val bv_bv_OpNe:BinaryOp[BitVector, BitVector, OpNe, BitVector] = new BinaryOp[BitVector, BitVector, OpNe, BitVector] {
    def apply(a: BitVector, b: BitVector): BitVector = {
      a ^^ b
    }
  }

  implicit val bv_bv_OpEq:BinaryOp[BitVector, BitVector, OpEq, BitVector] = new BinaryOp[BitVector, BitVector, OpEq, BitVector] {
    def apply(a: BitVector, b: BitVector): BitVector = {
      if(!a.lengthsMatch(b)) throw new IllegalArgumentException(s"Lengths don't match: ${a.length} ${b.length}")
      !(a :!= b)
    }
  }
}