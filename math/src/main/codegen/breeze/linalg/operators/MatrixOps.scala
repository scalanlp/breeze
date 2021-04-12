package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.generic.UFunc.UImpl2
import shapeless.=:!=

import scala.reflect.ClassTag
import breeze.linalg.support.{LiteralRow, CanCopy}
import breeze.storage.Zero
import breeze.math.{Field, Ring, Complex, Semiring}
import breeze.macros.expand
import scala.math.BigInt
import breeze.linalg._

trait MatrixGenericOps { this: Matrix.type =>
  class SetMMOp[@specialized(Double, Int, Float, Long) V, MM](implicit subtype: MM <:< Matrix[V])
      extends OpSet.InPlaceImpl2[Matrix[V], MM] {
    def apply(a: Matrix[V], b: MM): Unit = {
      require(a.rows == b.rows, "Row dimension mismatch!")
      require(a.cols == b.cols, "Col dimension mismatch!")
      val bb = subtype(b)
      // TODO: might make sense to have a "am I sparse?" check and use activeIterator instead?
      for (i <- 0 until a.rows; j <- 0 until a.cols) {
        a(i, j) = bb(i, j)
      }

    }
  }
  implicit def setDMDV[V, MM](implicit st: MM <:< Matrix[V]) = new SetMMOp[V, MM]

  implicit def canCopyMatrix[V: ClassTag] = new CanCopy[Matrix[V]] {
    def apply(v1: Matrix[V]) = {
      v1.copy
    }
  }

  import breeze.math.PowImplicits._

  implicit def m_m_OpAdd_Update_Semi[T: Semiring: ClassTag: Zero]: OpAdd.InPlaceImpl2[Matrix[T], Matrix[T]] =
    new OpAdd.InPlaceImpl2[Matrix[T], Matrix[T]] {
      override def apply(a: Matrix[T], b: Matrix[T]): Unit = {
        val ring = implicitly[Semiring[T]]
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = ring.+(a(r, c), b(r, c))
            r += 1
          }
          c += 1
        }

      }
    }

  implicit def m_m_OpMul_Update_Semi[T: Semiring: ClassTag: Zero]: OpMulScalar.InPlaceImpl2[Matrix[T], Matrix[T]] =
    new OpMulScalar.InPlaceImpl2[Matrix[T], Matrix[T]] {
      override def apply(a: Matrix[T], b: Matrix[T]): Unit = {
        val ring = implicitly[Semiring[T]]
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = ring.*(a(r, c), b(r, c))
            r += 1
          }
          c += 1
        }

      }
    }

  implicit def m_m_OpSub_Update_Ring[T: Ring: ClassTag: Zero]: OpSub.InPlaceImpl2[Matrix[T], Matrix[T]] =
    new OpSub.InPlaceImpl2[Matrix[T], Matrix[T]] {
      override def apply(a: Matrix[T], b: Matrix[T]): Unit = {
        val ring = implicitly[Ring[T]]
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = ring.-(a(r, c), b(r, c))
            r += 1
          }
          c += 1
        }

      }
    }

  implicit def m_m_OpDiv_Update_Ring[T: Field: ClassTag: Zero]: OpDiv.InPlaceImpl2[Matrix[T], Matrix[T]] =
    new OpDiv.InPlaceImpl2[Matrix[T], Matrix[T]] {
      override def apply(a: Matrix[T], b: Matrix[T]): Unit = {
        val ring = implicitly[Field[T]]
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = ring./(a(r, c), b(r, c))
            r += 1
          }
          c += 1
        }

      }
    }

  implicit def castOps[M1, M2, T, Op, MR](
      implicit v1ev: M1 <:< Matrix[T],
      v1ne: M1 =:!= Matrix[T],
      v2ev: M2 <:< Matrix[T],
      v2ne: M2 =:!= Matrix[T],
      op: UImpl2[Op, Matrix[T], Matrix[T], MR]): UImpl2[Op, M1, M2, MR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, M1, M2, MR]]
  }

  implicit def castUpdateOps[M1, M2, T, Op <: OpType](
      implicit v1ev: M1 <:< Matrix[T],
      v2ev: M2 <:< Matrix[T],
      op: UFunc.InPlaceImpl2[Op, Matrix[T], Matrix[T]]): UFunc.InPlaceImpl2[Op, M1, M2] = {
    op.asInstanceOf[UFunc.InPlaceImpl2[Op, M1, M2]]
  }
}

trait MatrixOps extends MatrixGenericOps { this: Matrix.type =>

  import breeze.math.PowImplicits._

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def m_m_UpdateOp[
      @expand.args(Int, Double, Float, Long, BigInt, Complex) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] =
    new BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] {
      override def bindingMissing(a: Matrix[T], b: Matrix[T]): Unit = {
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = op(a(r, c), b(r, c))
            r += 1
          }
          c += 1
        }

      }
    }

  @expand
  implicit def m_m_UpdateOp[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType,
      T: Field: Zero: ClassTag](
      implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f.*(_, _) }, { f./(_, _) }, {  (__x, __y) => __y }, { f.%(_, _) }, { f.pow(_, _) }) op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] =
    new BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type] {
      val f = implicitly[Field[T]]
      override def bindingMissing(a: Matrix[T], b: Matrix[T]): Unit = {
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = op(a(r, c), b(r, c))
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
  implicit def m_s_UpdateOp[
      @expand.args(Int, Double, Float, Long, BigInt, Complex) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Matrix[T], T, Op.type] =
    new BinaryUpdateRegistry[Matrix[T], T, Op.type] {
      override def bindingMissing(a: Matrix[T], b: T): Unit = {
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = op(a(r, c), b)
            r += 1
          }
          c += 1
        }

      }
    }

  @expand
  implicit def m_s_UpdateOp[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType,
      T: Field: Zero: ClassTag](
      implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f.*(_, _) }, { f.*(_, _) }, { f./(_, _) }, {
        f.%(_, _)
      }, { f.pow(_, _) }) op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Matrix[T], T, Op.type] =
    new BinaryUpdateRegistry[Matrix[T], T, Op.type] {
      val f = implicitly[Field[T]]
      override def bindingMissing(a: Matrix[T], b: T): Unit = {
        var c = 0

        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            a(r, c) = op(a(r, c), b)
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
  implicit def op_M_S[
      @expand.args(Int, Long, Float, Double, BigInt, Complex) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpMod, OpDiv, OpPow) Op]
    : BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[Matrix[T], T]]
    new BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]] {
      override def bindingMissing(a: Matrix[T], b: T) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      //      implicitly[BinaryRegistry[Matrix[T], T, Op, Matrix[T]]].register(this)
    }
  }

  @expand
  implicit def op_M_S[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType,
      T: Field: Zero: ClassTag]
//  (implicit @expand.sequence[Op]({f.+(_,_)}, {f.-(_,_)}, {f.*(_,_)}, {f.*(_,_)}, {f./(_,_)}, {f.%(_,_)},{f.pow(_,_)}) op: Op.Impl2[T,T,T])
    : BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[Matrix[T], T]]
    new BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]] {
      override def bindingMissing(a: Matrix[T], b: T) = {
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
  implicit def op_S_M[
      @expand.args(Int, Long, Float, Double, BigInt, Complex) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op](implicit @expand.sequence[Op]({
    _ + _
  }, { _ - _ }, { _ * _ }, { _ * _ }, { _ / _ }, { _ % _ }, { _.pow(_) }) op: Op.Impl2[T, T, T])
    : BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]] = {
    new BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]] {
      override def bindingMissing(b: T, a: Matrix[T]) = {
        val res = DenseMatrix.zeros[T](a.rows, a.cols)
        val resd = res.data
        var c = 0

        var off = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            resd(off) = op(a(r, c), b)
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
  implicit def op_S_M[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType,
      T: Field: Zero: ClassTag](
      implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f.*(_, _) }, { f.*(_, _) }, { f./(_, _) }, {
        f.%(_, _)
      }, { f.pow(_, _) }) op: Op.Impl2[T, T, T]): BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]] = {
    val f = implicitly[Field[T]]
    new BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]] {
      override def bindingMissing(b: T, a: Matrix[T]) = {
        val res = DenseMatrix.zeros[T](a.rows, a.cols)
        val resd = res.data
        var c = 0

        var off = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            resd(off) = op(a(r, c), b)
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
  implicit def op_M_DM[
      @expand.args(Int, Long, Float, Double, BigInt, Complex) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMod, OpDiv, OpPow) Op]
    : BinaryRegistry[Matrix[T], Matrix[T], Op.type, Matrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[Matrix[T], Matrix[T]]]
    new BinaryRegistry[Matrix[T], Matrix[T], Op.type, Matrix[T]] {
      override def bindingMissing(a: Matrix[T], b: Matrix[T]) = {
        val c = copy(a)
        uop(c, b)
        c
      }
    }
  }

}

trait MatrixOpsLowPrio extends MatrixGenericOps { this: MatrixOps with Matrix.type =>
  implicit def canMulM_V_def[T, B <: Vector[T]](
      implicit bb: B <:< Vector[T],
      op: OpMulMatrix.Impl2[Matrix[T], Vector[T], Vector[T]]) = (
    implicitly[OpMulMatrix.Impl2[Matrix[T], Vector[T], Vector[T]]]
      .asInstanceOf[breeze.linalg.operators.OpMulMatrix.Impl2[Matrix[T], B, Vector[T]]]
    )

  // ibid.
  implicit def canMulM_M_def[T, B <: Matrix[T]](
      implicit bb: B <:< Matrix[T],
      op: OpMulMatrix.Impl2[Matrix[T], Matrix[T], Matrix[T]]) = (
    op.asInstanceOf[OpMulMatrix.Impl2[Matrix[T], B, Matrix[T]]]
  )
}

trait MatrixMultOps extends MatrixOps with MatrixOpsLowPrio { this: Matrix.type =>
  @expand
  @expand.valify
  implicit def op_M_V[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]
    : BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]] =
    new BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]] {
      override def bindingMissing(a: Matrix[T], b: Vector[T]): Vector[T] = {

        require(a.cols == b.length)
        val res = Vector.zeros[T](a.rows)
        var c = 0
        while (c < a.cols) {
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
    }

  implicit def op_M_V_Semiring[T: Semiring: Zero: ClassTag]: OpMulMatrix.Impl2[Matrix[T], Vector[T], Vector[T]] =
    new OpMulMatrix.Impl2[Matrix[T], Vector[T], Vector[T]] {
      override def apply(a: Matrix[T], b: Vector[T]): Vector[T] = {
        val ring = implicitly[Semiring[T]]

        require(a.cols == b.length)
        val res = Vector.zeros[T](a.rows)
        var c = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            val v = a(r, c)
            res(r) = ring.+(res(r), ring.*(v, b(c)))
            r += 1
          }
          c += 1
        }

        res
      }
    }
  @expand
  @expand.valify
  implicit def op_M_M[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]
    : BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]] =
    new BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]] {
      override def bindingMissing(a: Matrix[T], b: Matrix[T]): Matrix[T] = {

        // TODO: this could probably be much faster
        val res = Matrix.zeros[T](a.rows, b.cols)
        require(a.cols == b.rows)
        var c = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            val v = a(r, c)
            var j = 0
            while (j < b.cols) {
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

  implicit def op_M_M_Semiring[T: Semiring: Zero: ClassTag]: OpMulMatrix.Impl2[Matrix[T], Matrix[T], Matrix[T]] =
    new OpMulMatrix.Impl2[Matrix[T], Matrix[T], Matrix[T]] {
      override def apply(a: Matrix[T], b: Matrix[T]): Matrix[T] = {
        val ring = implicitly[Semiring[T]]

        // TODO: this could probably be much faster
        val res = Matrix.zeros[T](a.rows, b.cols)
        require(a.cols == b.rows)
        var c = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            val v = a(r, c)
            var j = 0
            while (j < b.cols) {
              res(r, j) = ring.+(res(r, j), ring.*(v, b(c, j)))
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
