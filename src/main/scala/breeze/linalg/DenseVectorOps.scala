package breeze.linalg

import operators._
import breeze.generic._
import breeze.linalg.support._
import breeze.math.{Complex, TensorSpace, Semiring, Ring}
import breeze.util.{ArrayUtil, Isomorphism}
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}
import breeze.macros.expand
import breeze.numerics.IntMath
import scala.math.BigInt

trait DenseVectorOps extends DenseVector_GenericOps { this: DenseVector.type =>
  import breeze.math.PowImplicits._

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  @expand.valify
  implicit def dv_dv_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[DenseVector[T], DenseVector[T], Op, DenseVector[T]] = {
    new BinaryOp[DenseVector[T], DenseVector[T], Op, DenseVector[T]] {
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
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op, Vector[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_v_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[DenseVector[T], Vector[T], Op, DenseVector[T]] = new BinaryOp[DenseVector[T], Vector[T], Op, DenseVector[T]] {
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
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_s_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[DenseVector[T], T, Op, DenseVector[T]] = new BinaryOp[DenseVector[T], T, Op, DenseVector[T]] {
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
    implicitly[BinaryRegistry[Vector[T], T, Op, Vector[T]]].register(this)
  }



  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_dv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseVector[T], DenseVector[T], Op] = new BinaryUpdateOp[DenseVector[T], DenseVector[T], Op] {
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
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_s_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseVector[T], T, Op] = new BinaryUpdateOp[DenseVector[T], T, Op] {
    def apply(a: DenseVector[T], b: T):Unit = {
      val ad = a.data
      var aoff = a.offset

      var i = 0
      while(i < a.length) {
        ad(aoff) = op(ad(aoff), b)
        aoff += a.stride
        i += 1
      }
      implicitly[BinaryUpdateRegistry[Vector[T], T, Op]].register(this)
    }
  }



  @expand
  @expand.valify
  implicit def canDot_DV_DV[@expand.args(Int, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0l, BigInt(0), Complex.zero) zero: T): BinaryOp[DenseVector[T], DenseVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[DenseVector[T], DenseVector[T], breeze.linalg.operators.OpMulInner, T] {
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
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner, T]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def canDot_DV_V[@expand.args(Int, Double, Float, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T): BinaryOp[DenseVector[T], Vector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[DenseVector[T], Vector[T], breeze.linalg.operators.OpMulInner, T] {
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
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner, T]].register(this)
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


  /**Returns the k-norm of this Vector. */
  @expand
  implicit def canNorm[@expand.args(Int, Double, Float, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T): CanNorm[DenseVector[T], Double] = {

    new CanNorm[DenseVector[T], Double] {
      def apply(v: DenseVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          foreach (v => sum += v.abs.toDouble )
          sum
        } else if (n == 2) {
          var sum = 0.0
          foreach (v => { val nn = v.abs.toDouble; sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          foreach (v => { val nn = v.abs.toDouble; if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          foreach (v => { val nn = v.abs.toDouble; sum += math.pow(nn,n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

}