package breeze.linalg.operators

import scala.reflect.ClassTag
import breeze.linalg.support.{LiteralRow, CanCopy}
import breeze.storage.DefaultArrayValue
import breeze.math.{Complex, Semiring}
import scala.specialized
import scala.util.Random
import breeze.macros.expand
import scala.math.BigInt
import breeze.linalg._


trait MatrixGenericOps { this: Matrix.type =>
  class SetMMOp[@specialized(Int, Double, Float) V, MM](implicit subtype: MM<:<Matrix[V]) extends OpSet.InPlaceImpl2[Matrix[V], MM] {
    def apply(a: Matrix[V], b: MM) {
      require(a.rows == b.rows, "Row dimension mismatch!")
      require(a.cols == b.cols, "Col dimension mismatch!")
      val bb = subtype(b)
      // TODO: might make sense to have a "am I sparse?" check and use activeIterator instead?
      for(i <- 0 until a.rows; j <- 0 until a.cols) {
        a(i,j) = bb(i, j)
      }

    }
  }
  implicit def setDMDV[V, MM](implicit st: MM<:<Matrix[V]) = new SetMMOp[V, MM]

  implicit def canCopyMatrix[V:ClassTag] = new CanCopy[Matrix[V]] {
    def apply(v1: Matrix[V]) = {
      v1.copy
    }
  }
}




trait MatrixOps extends MatrixGenericOps { this: Matrix.type =>

  import breeze.math.PowImplicits._

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def m_m_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] = new BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] {
    override def bindingMissing(a: Matrix[T], b: Matrix[T]):Unit = {
      var c = 0

      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          a(r, c) = op(a(r,c), b(r,c))
          r += 1
        }
        c += 1
      }

    }
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def m_s_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):BinaryUpdateRegistry[Matrix[T], T, Op.type] = new BinaryUpdateRegistry[Matrix[T], T, Op.type] {
    override def bindingMissing(a: Matrix[T], b: T):Unit = {
      var c = 0

      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          a(r, c) = op(a(r,c), b)
          r += 1
        }
        c += 1
      }

    }
  }


  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def op_M_S[@expand.args(Int, Long, Float, Double, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpMod, OpDiv, OpPow) Op]: BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[Matrix[T], T]]
    new BinaryRegistry[Matrix[T],  T, Op.type, Matrix[T]] {
      override def bindingMissing(a : Matrix[T], b: T) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      //      implicitly[BinaryRegistry[Matrix[T], T, Op, Matrix[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def op_S_M[@expand.args(Int, Long, Float, Double, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod,  OpPow) Op]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T])
  : BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]] = {
    new BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]] {
      override def bindingMissing(b: T, a : Matrix[T]) = {
        val res = DenseMatrix.zeros[T](a.rows,a.cols)
        val resd = res.data
        var c = 0

        var off = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            resd(off) = op(a(r,c), b)
            r += 1
            off += 1
          }
          c += 1
        }
        res
      }
    }
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def op_M_DM[@expand.args(Int, Long, Float, Double, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMod, OpDiv, OpPow) Op]: BinaryRegistry[Matrix[T], Matrix[T], Op.type, Matrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[Matrix[T], Matrix[T]]]
    new BinaryRegistry[Matrix[T],  Matrix[T], Op.type, Matrix[T]] {
      override def bindingMissing(a : Matrix[T], b: Matrix[T]) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      //      implicitly[BinaryRegistry[Matrix[T], Matrix[T], Op, Matrix[T]]].register(this)
    }
  }

}

trait MatrixOpsLowPrio extends MatrixGenericOps { this: MatrixOps with Matrix.type =>
  @expand
  implicit def canMulM_V_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, B](implicit bb :  B <:< Vector[T]):OpMulMatrix.Impl2[Matrix[T], B, Vector[T]] = (
    implicitly[OpMulMatrix.Impl2[Matrix[T], Vector[T], Vector[T]]].asInstanceOf[breeze.linalg.operators.OpMulMatrix.Impl2[Matrix[T], B, Vector[T]]]
    )

  @expand
  implicit def canMulM_M_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, B](implicit bb :  B <:< Matrix[T]):OpMulMatrix.Impl2[Matrix[T], B, Matrix[T]] = (
    implicitly[OpMulMatrix.Impl2[Matrix[T], Matrix[T], Matrix[T]]].asInstanceOf[OpMulMatrix.Impl2[Matrix[T], B, Matrix[T]]]
    )
}

trait MatrixMultOps extends MatrixOps with MatrixOpsLowPrio { this: Matrix.type =>
  @expand
  @expand.valify
  implicit def op_M_V[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]] = new BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]] {
    override def bindingMissing(a: Matrix[T], b: Vector[T]) = {

      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = Vector.zeros[T](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) += v * b(c)
          r += 1
        }
        c += 1
      }

      res
    }
  };


  @expand
  @expand.valify
  implicit def op_M_M[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]] = new BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]] {
    override def bindingMissing(a: Matrix[T], b: Matrix[T]) = {

      // TODO: this could probably be much faster
      val res = Matrix.zeros[T](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res
    }
  }

}