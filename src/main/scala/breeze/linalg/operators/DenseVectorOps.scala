package breeze.linalg.operators

import breeze.linalg._

import breeze.generic._
import breeze.linalg.support._
import breeze.math.{Complex, Semiring}
import breeze.util.ArrayUtil
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import breeze.macros.expand
import scala.math.BigInt
import breeze.math.PowImplicits._

trait DenseVectorOps extends DenseVector_GenericOps { this: DenseVector.type =>

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  @expand.valify
  implicit def dv_dv_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] = {
    new Op.Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] {
      def apply(a: DenseVector[T], b: DenseVector[T]): DenseVector[T] = {
        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset
        val result = DenseVector.zeros[T](a.length)
        val rd = result.data

        var i = 0
        while(i < a.length) {
          rd(i) = op(ad(aoff), bd(boff))
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_v_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.Impl2[DenseVector[T], Vector[T], DenseVector[T]] = new Op.Impl2[DenseVector[T], Vector[T], DenseVector[T]] {
    def apply(a: DenseVector[T], b: Vector[T]): DenseVector[T] = {
      val ad = a.data
      var aoff = a.offset
      val result = DenseVector.zeros[T](a.length)
      val rd = result.data

      var i = 0
      while(i < a.length) {
        rd(i) = op(ad(aoff), b(i))
        aoff += a.stride
        i += 1
      }
      result
    }
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_s_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.Impl2[DenseVector[T], T, DenseVector[T]] = new Op.Impl2[DenseVector[T], T, DenseVector[T]] {
    def apply(a: DenseVector[T], b: T): DenseVector[T] = {
      val ad = a.data
      var aoff = a.offset
      val result = DenseVector.zeros[T](a.length)
      val rd = result.data

      var i = 0
      while(i < a.length) {
        rd(i) = op(ad(aoff), b)
        aoff += a.stride
        i += 1
      }
      result
    }
    implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
  }



  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_dv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[DenseVector[T], DenseVector[T]] = new Op.InPlaceImpl2[DenseVector[T], DenseVector[T]] {
    def apply(a: DenseVector[T], b: DenseVector[T]):Unit = {
      val ad = a.data
      val bd = b.data
      var aoff = a.offset
      var boff = b.offset

      var i = 0
      while(i < a.length) {
        ad(aoff) = op(ad(aoff), bd(boff))
        aoff += a.stride
        boff += b.stride
        i += 1
      }
    }
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_s_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _})
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[DenseVector[T], T] = new Op.InPlaceImpl2[DenseVector[T], T] {
    def apply(a: DenseVector[T], b: T):Unit = {
      val ad = a.data
      var aoff = a.offset

      var i = 0
      while(i < a.length) {
        ad(aoff) = op(ad(aoff), b)
        aoff += a.stride
        i += 1
      }
      implicitly[BinaryUpdateRegistry[Vector[T], T, Op.type]].register(this)
    }
  }



  @expand
  @expand.valify
  implicit def canDot_DV_DV[@expand.args(Int, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0l, BigInt(0), Complex.zero) zero: T): breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], DenseVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], DenseVector[T], T] {
      def apply(a: DenseVector[T], b: DenseVector[T]) = {
        require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset
        var result : T = zero

        var i = 0
        while(i < a.length) {
          result += ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        result

      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def canDot_DV_V[@expand.args(Int, Double, Float, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T): breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], Vector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], Vector[T], T] {
      def apply(a: DenseVector[T], b: Vector[T]) = {
        require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        var aoff = a.offset
        var result : T = zero

        var i = 0
        while(i < a.length) {
          result += ad(aoff) * b(i)
          aoff += a.stride
          i += 1
        }
        result

      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }

  }


  @expand
  implicit def axpy[@expand.args(Int, Double, Float, Long, BigInt, Complex) V]: CanAxpy[V, DenseVector[V], DenseVector[V]] = {
    new CanAxpy[V, DenseVector[V], DenseVector[V]] {
      def apply(s: V, b: DenseVector[V], a: DenseVector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")
        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) += s * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }







}



/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVector_SpecialOps extends DenseVectorOps { this: DenseVector.type =>


  implicit val canDot_DV_DV_Float: breeze.linalg.operators.OpMulInner.Impl2[DenseVector[Float], DenseVector[Float], Float] = {
    new breeze.linalg.operators.OpMulInner.Impl2[DenseVector[Float], DenseVector[Float], Float] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        blas.sdot(
          a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      implicitly[BinaryRegistry[Vector[Float], Vector[Float], OpMulInner.type, Float]].register(this)
    }
  }
}

/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVector_OrderingOps extends DenseVectorOps { this: DenseVector.type =>

  @expand
  implicit def dv_dv_Op[@expand.args(Int, Double, Float, Long, BigInt) T,
  @expand.args(OpGT, OpGTE, OpLTE, OpLT, OpEq, OpNe) Op <: OpType]
  (implicit @expand.sequence[Op]({_ > _},  {_ >= _}, {_ <= _}, {_ < _}, { _ == _}, {_ != _})
  op: Op.Impl2[T, T, T]):Op.Impl2[DenseVector[T], DenseVector[T], BitVector] = new Op.Impl2[DenseVector[T], DenseVector[T], BitVector] {
    def apply(a: DenseVector[T], b: DenseVector[T]): BitVector = {
      if(a.length != b.length) throw new ArrayIndexOutOfBoundsException(s"Lengths don't match for operator $Op ${a.length} ${b.length}")
      val ad = a.data
      val bd = b.data
      var aoff = a.offset
      var boff = b.offset
      val result = BitVector.zeros(a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(ad(aoff), bd(boff))
        aoff += a.stride
        boff += b.stride
        i += 1
      }
      result
    }
  }

  @expand
  implicit def dv_v_Op[@expand.args(Int, Double, Float, Long, BigInt) T,
  @expand.args(OpGT, OpGTE, OpLTE, OpLT, OpEq, OpNe) Op <: OpType]
  (implicit @expand.sequence[Op]({_ > _},  {_ >= _}, {_ <= _}, {_ < _}, { _ == _}, {_ != _})
  op: Op.Impl2[T, T, Boolean]):Op.Impl2[DenseVector[T], Vector[T], BitVector] = new Op.Impl2[DenseVector[T], Vector[T], BitVector] {
    def apply(a: DenseVector[T], b: Vector[T]): BitVector = {
      val ad = a.data
      var aoff = a.offset
      val result = BitVector.zeros(a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(ad(aoff), b(i))
        aoff += a.stride
        i += 1
      }
      result
    }
  }


  @expand
  implicit def dv_s_CompOp[@expand.args(Int, Double, Float, Long, BigInt) T,
  @expand.args(OpGT, OpGTE, OpLTE, OpLT, OpEq, OpNe) Op <: OpType]
  (implicit @expand.sequence[Op]({_ > _},  {_ >= _}, {_ <= _}, {_ < _}, { _ == _}, {_ != _})
  op: Op.Impl2[T, T, Boolean]):Op.Impl2[DenseVector[T], T, BitVector] = new Op.Impl2[DenseVector[T], T, BitVector] {
    def apply(a: DenseVector[T], b: T): BitVector = {
      val ad = a.data
      var aoff = a.offset
      val result = BitVector.zeros(a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(ad(aoff), b)
        aoff += a.stride
        i += 1
      }
      result
    }
  }





}

/**
 * TODO
 *
 * @author dlwh
 **/
trait DenseVector_GenericOps extends UFunc2ZippingImplicits[DenseVector] { this: DenseVector.type =>
  @expand
  implicit def pureFromUpdate[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  Other,
  Op<:OpType](op: UFunc.InPlaceImpl2[Op, DenseVector[T], Other])
             (implicit copy: CanCopy[DenseVector[T]]): UFunc.UImpl2[Op, DenseVector[T], Other, DenseVector[T]] = {
    new  UFunc.UImpl2[Op, DenseVector[T], Other, DenseVector[T]] {
      override def apply(a : DenseVector[T], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def pureFromUpdate[T, Other, Op<:OpType](op: UFunc.InPlaceImpl2[Op, DenseVector[T], Other])
                                                   (implicit copy: CanCopy[DenseVector[T]]): UFunc.UImpl2[Op, DenseVector[T], Other, DenseVector[T]] = {
    new  UFunc.UImpl2[Op, DenseVector[T], Other, DenseVector[T]] {
      override def apply(a : DenseVector[T], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }



  implicit def canSet_DV_Generic[V]: OpSet.InPlaceImpl2[DenseVector[V], V] = {
    new OpSet.InPlaceImpl2[DenseVector[V], V] {
      def apply(a: DenseVector[V], b: V) {
        val ad = a.data
        if(a.stride == 1) {
          ArrayUtil.fill(ad, a.offset, a.length, b)
          return
        }

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }

      }
    }
  }

  implicit def canSet_DV_DV_Generic[V]: breeze.linalg.operators.OpSet.InPlaceImpl2[DenseVector[V], DenseVector[V]] = {
    new OpSet.InPlaceImpl2[DenseVector[V], DenseVector[V]] {
      def apply(a: DenseVector[V], b: DenseVector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")
        if(a.stride == b.stride && a.stride == 1) {
          System.arraycopy(b.data, b.offset, a.data, a.offset, a.length)
          return
        }

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }

      }
    }
  }

  implicit def canGaxpy[V:Semiring]: CanAxpy[V, DenseVector[V], DenseVector[V]] = {
    new CanAxpy[V, DenseVector[V], DenseVector[V]] {
      val ring = implicitly[Semiring[V]]
      def apply(s: V, b: DenseVector[V], a: DenseVector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")
        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ring.+(ad(aoff),ring.*(s, bd(boff)))
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit def dv_v_OpSet[T, Vec](implicit ev: Vec <:< Vector[T]):OpSet.InPlaceImpl2[DenseVector[T], Vec] = new OpSet.InPlaceImpl2[DenseVector[T], Vec] {
    def apply(a: DenseVector[T], b: Vec):Unit = {
      val ad = a.data
      var aoff = a.offset

      var i = 0
      while(i < a.length) {
        ad(aoff) = b(i)
        aoff += a.stride
        i += 1
      }
    }
//    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], OpSet.type]].register(this)
  }



}