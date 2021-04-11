package breeze.linalg.operators

import breeze.linalg.{BitVector, DenseVector, SparseVector, Vector, all, any, scaleAdd}
import breeze.macros.expand
import breeze.math.{Complex, Semiring}

import java.util
import scala.math.BigInt

trait BitVectorOps {

  implicit object anyImpl extends any.Impl[BitVector, Boolean] {
    override def apply(v: BitVector): Boolean = {
      v.data.cardinality() != 0
    }
  }

  implicit object allImpl extends all.Impl[BitVector, Boolean] {
    override def apply(v: BitVector): Boolean = {
      v.data.cardinality() == v.size
    }
  }

  @expand
  @expand.valify
  implicit def bv_bv_UpdateOp[@expand.args(OpAnd, OpOr, OpXor) Op <: OpType](
                                                                                     implicit @expand.sequence[Op]({
    _.and(_)
  }, {
    _.or(_)
  }, {
    _.xor(_)
  })
  op: Op.InPlaceImpl2[java.util.BitSet, java.util.BitSet]): Op.InPlaceImpl2[BitVector, BitVector] =
    new Op.InPlaceImpl2[BitVector, BitVector] {
      def apply(a: BitVector, b: BitVector): Unit = {
        if (!a.lengthsMatch(b)) throw new IllegalArgumentException(s"Lengths don't match: ${a.length} ${b.length}")
        op(a.data, b.data)
      }
    }


  implicit val bv_bv_UpdateOp_OpSet: OpSet.InPlaceImpl2[BitVector, BitVector] =
    new OpSet.InPlaceImpl2[BitVector, BitVector] {
      def apply(a: BitVector, b: BitVector): Unit = {
        if (!a.lengthsMatch(b)) throw new IllegalArgumentException(s"Lengths don't match: ${a.length} ${b.length}")
        a.data.clear()
        a.data.or(b.data)
      }
    }

  @expand
  @expand.valify
  implicit def bv_bv_Op[@expand.args(OpAnd, OpOr, OpXor) Op <: OpType](
                                                                        implicit @expand.sequence[Op]({
    _.and(_)
  }, {
    _.or(_)
  }, {
    _.xor(_)
  })
  op: Op.InPlaceImpl2[java.util.BitSet, java.util.BitSet]): Op.Impl2[BitVector, BitVector, BitVector] =
    new Op.Impl2[BitVector, BitVector, BitVector] {
      def apply(a: BitVector, b: BitVector) = {
        if (!a.lengthsMatch(b)) throw new IllegalArgumentException(s"Lengths don't match: ${a.length} ${b.length}")
        val result = a.data.clone().asInstanceOf[util.BitSet]
        op(result, b.data)
        new BitVector(result, a.length.max(b.length), a.enforceLength && b.enforceLength)
      }
    }

  implicit val bv_OpNot: OpNot.Impl[BitVector, BitVector] = new OpNot.Impl[BitVector, BitVector] {
    def apply(a: BitVector): BitVector = {
      val ones = BitVector.ones(a.length, a.enforceLength)
      ones.data.andNot(a.data)
      ones
    }
  }

  implicit val bv_bv_OpNe: OpNe.Impl2[BitVector, BitVector, BitVector] =
    new OpNe.Impl2[BitVector, BitVector, BitVector] {
      def apply(a: BitVector, b: BitVector): BitVector = {
        a ^^ b
      }
    }

  implicit val bv_bv_OpEq: OpEq.Impl2[BitVector, BitVector, BitVector] =
    new OpEq.Impl2[BitVector, BitVector, BitVector] {
      def apply(a: BitVector, b: BitVector): BitVector = {
        if (!a.lengthsMatch(b)) throw new IllegalArgumentException(s"Lengths don't match: ${a.length} ${b.length}")
        !(a :!= b)
      }
    }

  @expand
  implicit def axpy[@expand.args(Int, Double, Float, Long) V, Vec](
                                                                    implicit ev: Vec <:< Vector[V]): scaleAdd.InPlaceImpl3[Vec, V, BitVector] = {
    new scaleAdd.InPlaceImpl3[Vec, V, BitVector] {
      def apply(a: Vec, s: V, b: BitVector): Unit = {
        require(b.lengthsMatch(a), "Vectors must be the Same length!")
        val bd = b.data
        var i = bd.nextSetBit(0)
        while (i >= 0) {
          a(i) += s
          i = bd.nextSetBit(i + 1)
        }
      }
    }
  }

  implicit def axpyGen[V, Vec](
                                implicit ev: Vec <:< Vector[V],
                                semi: Semiring[V]): scaleAdd.InPlaceImpl3[Vec, V, BitVector] = {
    new scaleAdd.InPlaceImpl3[Vec, V, BitVector] {
      def apply(a: Vec, s: V, b: BitVector): Unit = {
        require(b.lengthsMatch(a), "Vectors must be the same length!")
        val bd = b.data
        var i = bd.nextSetBit(0)
        while (i >= 0) {
          a(i) = semi.+(a(i), s)
          i = bd.nextSetBit(i + 1)
        }
      }
    }
  }

  implicit val canDot_BV_BV: OpMulInner.Impl2[BitVector, BitVector, Boolean] = {
    new breeze.linalg.operators.OpMulInner.Impl2[BitVector, BitVector, Boolean] {
      def apply(a: BitVector, b: BitVector): Boolean = {
        require(a.lengthsMatch(b), "Vectors must be the same length!")
        a.data.intersects(b.data)
      }
    }
  }

  @expand
  @expand.valify
  implicit def canDot_BV_DenseVector[@expand.args(Double, Float, Int, Long) T](
                                                                                implicit @expand.sequence[T](0.0, 0.0f, 0, 0L) zero: T)
  : breeze.linalg.operators.OpMulInner.Impl2[BitVector, DenseVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[BitVector, DenseVector[T], T] {
      def apply(a: BitVector, b: DenseVector[T]) = {
        val ad = a.data
        val boff = b.offset
        val bd = b.data
        val bstride = b.stride
        var result: T = zero

        var i = ad.nextSetBit(0)
        while (i >= 0) {
          result += bd(boff + bstride * i)
          i = ad.nextSetBit(i + 1)
        }
        result

      }
      //      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner, T]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def canDot_BV_SV[@expand.args(Int, Long, BigInt, Complex) T](
                                                                         implicit @expand.sequence[T](0, 0L, BigInt(0), Complex.zero) _zero: T)
  : breeze.linalg.operators.OpMulInner.Impl2[BitVector, SparseVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[BitVector, SparseVector[T], T] {
      def apply(a: BitVector, b: SparseVector[T]): T = {
        require(a.lengthsMatch(b), "Vectors must be the same length!")
        if (b.activeSize == 0) return _zero

        val ad = a.data
        var boff = 0
        var result: T = _zero
        while (boff < b.activeSize) {
          if (ad.get(b.indexAt(boff)))
            result += b.valueAt(boff)
          boff += 1
        }
        result

      }
      //      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner, T]].register(this)
    }
  }

  implicit def canDot_Other_BV[T, Other](
                                          implicit op: OpMulInner.Impl2[BitVector, Other, T]): OpMulInner.Impl2[Other, BitVector, T] = {
    new OpMulInner.Impl2[Other, BitVector, T] {
      def apply(a: Other, b: BitVector): T = {
        op(b, a)
      }
    }
  }

}
