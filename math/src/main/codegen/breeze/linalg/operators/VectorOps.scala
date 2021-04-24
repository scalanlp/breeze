package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.generic.UFunc.{InPlaceImpl2, UImpl, UImpl2}
import breeze.linalg.support.CanCopy
import breeze.linalg.{Vector, VectorBuilder, ZippedValues, scaleAdd, zipValues}
import breeze.macros.expand
import breeze.math.{Field, Ring, Semiring}
import breeze.storage.Zero

import scala.reflect.ClassTag

trait VectorOps extends GenericOps with Vector_TraversalOps {
 
  import breeze.math.PowImplicits._
  @expand.valify
  @expand
  implicit def v_v_Idempotent_Op[@expand.args(Int, Double, Float, Long) T, @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
      op: Op.Impl2[T, T, T]): BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] =
    new BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] {
      override def bindingMissing(a: Vector[T], b: Vector[T]): Vector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result = a.copy
        for ((k, v) <- b.activeIterator) {
          result(k) = op(a(k), v)
        }
        result
      }
    }

  implicit def v_v_Idempotent_OpSub[T: Ring]: OpSub.Impl2[Vector[T], Vector[T], Vector[T]] =
    new OpSub.Impl2[Vector[T], Vector[T], Vector[T]] {
      val r = implicitly[Ring[T]]
      def apply(a: Vector[T], b: Vector[T]): Vector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result = a.copy
        for ((k, v) <- b.activeIterator) {
          result(k) = r.-(a(k), v)
        }
        result
      }
    }

  implicit def v_v_Idempotent_OpAdd[T: Semiring]: OpAdd.Impl2[Vector[T], Vector[T], Vector[T]] =
    new OpAdd.Impl2[Vector[T], Vector[T], Vector[T]] {
      val r = implicitly[Semiring[T]]
      def apply(a: Vector[T], b: Vector[T]): Vector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result = a.copy
        for ((k, v) <- b.activeIterator) {
          result(k) = r.+(a(k), v)
        }
        result
      }
    }

  @expand
  @expand.valify
  implicit def v_v_nilpotent_Op[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T)
    : BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]] =
    new BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]] {
      override def bindingMissing(a: Vector[T], b: Vector[T]): Vector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val builder = new VectorBuilder[T](a.length)
        for ((k, v) <- b.activeIterator) {
          val r = a(k) * v
          if (r != zero)
            builder.add(k, r)
        }
        builder.toVector
      }
    }
  @expand
  @expand.valify
  implicit def v_v_Op[@expand.args(Int, Double, Float, Long) T, @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ }, { (_x, _y) =>
        _y
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] =
    new BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] {
      override def bindingMissing(a: Vector[T], b: Vector[T]): Vector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result = Vector.zeros[T](a.length)
        var i = 0
        while (i < a.length) {
          result(i) = op(a(i), b(i))
          i += 1
        }
        result
      }
    }

  /*
  @expand

  implicit def cast_v_v_Op[V1, V2,
  @expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar,OpDiv, OpSet, OpMod, OpPow) Op <: OpType](implicit v1: V1<:<Vector[T], v2: V2<:<Vector[T]) = {
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].asInstanceOf[Op.Impl2[V1, V2, Vector[T]]]
  }
   */

  @expand
  @expand.valify
  implicit def v_s_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ * _ }, { _ / _ }, { (_x, _y) =>
        _y
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T],
      @expand.sequence[T](0, 0.0, 0.0f, 0L)
      zero: T): BinaryRegistry[Vector[T], T, Op.type, Vector[T]] =
    new BinaryRegistry[Vector[T], T, Op.type, Vector[T]] {
      override def bindingMissing(a: Vector[T], b: T): Vector[T] = {
        val result = Vector.zeros[T](a.length)

        var i = 0
        while (i < a.length) {
          result(i) = op(a(i), b)
          i += 1
        }
        result
      }
    }

  @expand
  @expand.valify
  implicit def s_v_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ * _ }, { _ / _ }, { (_x, _y) =>
        _y
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T],
      @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T): BinaryRegistry[T, Vector[T], Op.type, Vector[T]] =
    new BinaryRegistry[T, Vector[T], Op.type, Vector[T]] {
      override def bindingMissing(b: T, a: Vector[T]): Vector[T] = {
        val result = Vector.zeros[T](a.length)

        var i = 0
        while (i < a.length) {
          result(i) = op(b, a(i))
          i += 1
        }
        result
      }
    }

  @expand
  implicit def v_sField_Op[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType,
      T: Field: ClassTag](implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f.*(_, _) }, { f.*(_, _) }, {
    f./(_, _)
  }, { f.%(_, _) }, { f.pow(_, _) }) op: Op.Impl2[T, T, T]): BinaryRegistry[Vector[T], T, Op.type, Vector[T]] =
    new BinaryRegistry[Vector[T], T, Op.type, Vector[T]] {
      val f = implicitly[Field[T]]
      override def bindingMissing(a: Vector[T], b: T): Vector[T] = {
        val result = Vector.zeros[T](a.length)

        var i = 0
        while (i < a.length) {
          result(i) = op(a(i), b)
          i += 1
        }
        result
      }
    }
  @expand
  @expand.valify
  implicit def v_v_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, { (_x, _y) =>
        _y
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] =
    new BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] {
      override def bindingMissing(a: Vector[T], b: Vector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        var i = 0
        while (i < a.length) {
          a(i) = op(a(i), b(i))
          i += 1
        }
      }
    }

  @expand
  @expand.valify
  implicit def v_v_Idempotent_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] =
    new BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] {
      override def bindingMissing(a: Vector[T], b: Vector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        for ((k, v) <- b.activeIterator) {
          a(k) = op(a(k), v)
        }
      }
    }

//  implicit def castUpdateOps[V1, V2, T, Op](
//      implicit v1ev: V1 <:< Vector[T],
//      V2ev: V2 <:< Vector[T],
//      op: UFunc.InPlaceImpl2[Op, Vector[T], Vector[T]]): InPlaceImpl2[Op, V1, V2] = {
//    op.asInstanceOf[UFunc.InPlaceImpl2[Op, V1, V2]]
//  }
//
//  implicit def castOps[V1, V2, T, Op, VR](
//      implicit v1ev: V1 <:< Vector[T],
//      V2ev: V2 <:< Vector[T],
//      op: UImpl2[Op, Vector[T], Vector[T], VR]): UImpl2[Op, V1, V2, VR] = {
//    op.asInstanceOf[UFunc.UImpl2[Op, V1, V2, VR]]
//  }

  //  implicit def castScalarOps[V1, T, Op, VR](implicit v1ev: V1<:<Vector[T],
  //                                            op: UImpl2[Op, Vector[T], T, VR]): UImpl2[Op, V1, T, VR] = {
  //    op.asInstanceOf[UFunc.UImpl2[Op, V1, T, VR]]
  //  }
  //
  //  implicit def castScalarLhsOps[V1, T, Op, VR](implicit v1ev: V1<:<Vector[T],
  //                                               op: UImpl2[Op, T, Vector[T], VR]): UImpl2[Op, T, V1, VR] = {
  //    op.asInstanceOf[UFunc.UImpl2[Op, T, V1, VR]]
  //  }

  import shapeless._
//
//  implicit def castFunc[V1, T, Op, VR](
//      implicit v1ev: V1 <:< Vector[T],
//      v1ne: V1 =:!= Vector[T],
//      op: UImpl[Op, Vector[T], VR]): UImpl[Op, V1, VR] = {
//    op.asInstanceOf[UFunc.UImpl[Op, V1, VR]]
//  }

  @expand
  @expand.valify
  implicit def v_s_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ * _ }, { _ / _ }, { (_x, _y) =>
        _y
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Vector[T], T, Op.type] =
    new BinaryUpdateRegistry[Vector[T], T, Op.type] {
      override def bindingMissing(a: Vector[T], b: T): Unit = {
        var i = 0
        while (i < a.length) {
          a(i) = op(a(i), b)
          i += 1
        }
      }
    }

  @expand
  implicit def v_s_UpdateOp[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType,
      T: Field: ClassTag](
      implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f.*(_, _) }, { f.*(_, _) }, { f./(_, _) }, {
        (_x, _y) =>
          _y
      }, { f.%(_, _) }, { f.pow(_, _) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[Vector[T], T, Op.type] =
    new BinaryUpdateRegistry[Vector[T], T, Op.type] {
      val f = implicitly[Field[T]]
      override def bindingMissing(a: Vector[T], b: T): Unit = {
        var i = 0
        while (i < a.length) {
          a(i) = op(a(i), b)
          i += 1
        }
      }
    }
  @expand
  @expand.valify
  implicit def canDot_V_V[@expand.args(Int, Long, Float, Double) T](
      implicit @expand.sequence[T](0, 0L, 0.0f, 0.0) zero: T)
    : BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] = {
    new BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] {
      override def bindingMissing(a: Vector[T], b: Vector[T]): T = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (a.activeSize > b.activeSize) {
          bindingMissing(b, a)
        } else {
          var result: T = zero
          for ((k, v) <- a.activeIterator) {
            result += v * b(k)
          }
          result
        }
      }
    }
  }

  implicit def canDot_V_V[T: ClassTag: Semiring]
    : BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] = {
    new BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] {
      val s = implicitly[Semiring[T]]
      override def bindingMissing(a: Vector[T], b: Vector[T]): T = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (a.activeSize > b.activeSize) {
          bindingMissing(b, a)
        } else {
          var result: T = s.zero
          for ((k, v) <- a.activeIterator) {
            result = s.+(result, s.*(v, b(k)))
          }
          result
        }
      }
    }
  }
  @expand
  @expand.valify
  implicit def impl_scaleAdd_InPlace_V_T_V[@expand.args(Int, Double, Float, Long) T]
    : TernaryUpdateRegistry[Vector[T], T, Vector[T], scaleAdd.type] = {
    new TernaryUpdateRegistry[Vector[T], T, Vector[T], scaleAdd.type] {
      override def bindingMissing(a: Vector[T], s: T, b: Vector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (s == 0) return

        var i = 0
        for ((k, v) <- b.activeIterator) {
          a(k) += s * v
          i += 1
        }
      }
    }
  }


  @expand
  @expand.valify
  implicit def zipValuesImpl_V_V[@expand.args(Int, Double, Float, Long) T]
    : BinaryRegistry[Vector[T], Vector[T], zipValues.type, ZippedValues[T, T]] = {
    new BinaryRegistry[Vector[T], Vector[T], zipValues.type, ZippedValues[T, T]] {
      protected override def bindingMissing(a: Vector[T], b: Vector[T]): ZippedValues[T, T] = {
        require(a.length == b.length, "vector dimension mismatch")
        ZippedVectorValues(a, b)
      }
    }
  }

  implicit def zipValuesSubclass[Vec1, Vec2, T, U](
      implicit view1: Vec1 <:< Vector[T],
      view2: Vec2 <:< Vector[U],
      op: zipValues.Impl2[Vector[T], Vector[U], ZippedValues[T, U]])
    : zipValues.Impl2[Vec1, Vec2, ZippedValues[T, U]] = {
    op.asInstanceOf[zipValues.Impl2[Vec1, Vec2, ZippedValues[T, U]]]
  }

  case class ZippedVectorValues[@specialized(Double, Int, Float, Long) T, @specialized(Double, Int, Float, Long) U](
      a: Vector[T],
      b: Vector[U])
      extends ZippedValues[T, U] {
    def foreach(f: (T, U) => Unit): Unit = {
      var i = 0
      while (i < a.length) {
        f(a(i), b(i))
        i += 1
      }
    }
  }

  implicit def vAddIntoField[T](
      implicit field: Field[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpAdd.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpAdd.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for (i <- 0 until v.length) v(i) = field.+(v(i), v2(i))
      }
    }

  }

  implicit def vSubIntoField[T](
      implicit field: Field[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpSub.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpSub.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for (i <- 0 until v.length) v(i) = field.-(v(i), v2(i))
      }
    }

  }

  implicit def vMulIntoField[T](
      implicit field: Field[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpMulScalar.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpMulScalar.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for (i <- 0 until v.length) v(i) = field.*(v(i), v2(i))
      }
    }

  }

  implicit def vDivIntoField[T](
      implicit field: Field[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpDiv.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpDiv.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for (i <- 0 until v.length) v(i) = field./(v(i), v2(i))
      }
    }

  }

  implicit def vPowInto[T](
      implicit pow: OpPow.Impl2[T, T, T],
      zero: Zero[T],
      ct: ClassTag[T]): OpPow.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpPow.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for (i <- 0 until v.length) v(i) = pow(v(i), v2(i))
      }
    }

  }

  implicit def vAddIntoSField[T](
      implicit field: Semiring[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpAdd.InPlaceImpl2[Vector[T], T] = {
    new OpAdd.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field.+(v(i), v2)
      }
    }
  }

  implicit def vSubIntoSField[T](
      implicit field: Ring[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpSub.InPlaceImpl2[Vector[T], T] = {
    new OpSub.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field.-(v(i), v2)
      }
    }

  }

  implicit def vMulScalarIntoSField[T](
      implicit field: Semiring[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpMulScalar.InPlaceImpl2[Vector[T], T] = {
    new OpMulScalar.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field.*(v(i), v2)
      }
    }
  }

  implicit def vDivIntoSField[T](
      implicit field: Field[T],
      zero: Zero[T],
      ct: ClassTag[T]): OpDiv.InPlaceImpl2[Vector[T], T] = {
    new OpDiv.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field./(v(i), v2)
      }
    }
  }

  implicit def vPowIntoS[T](
      implicit pow: OpPow.Impl2[T, T, T],
      zero: Zero[T],
      ct: ClassTag[T]): OpPow.InPlaceImpl2[Vector[T], T] = {
    new OpPow.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = pow(v(i), v2)
      }
    }
  }

  implicit def V_dotField[T](implicit field: Semiring[T]): OpMulInner.Impl2[Vector[T], Vector[T], T] = {
    new OpMulInner.Impl2[Vector[T], Vector[T], T] {
      override def apply(v: Vector[T], v2: Vector[T]): T = {
        var acc = field.zero
        for (i <- 0 until v.length) {
          acc = field.+(acc, field.*(v(i), v2(i)))
        }
        acc
      }
    }
  }

  implicit def implOpSet_V_V_InPlace[V]: OpSet.InPlaceImpl2[Vector[V], Vector[V]] = {

    new OpSet.InPlaceImpl2[Vector[V], Vector[V]] {
      def apply(a: Vector[V], b: Vector[V]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")

        for (i <- 0 until a.length) {
          a(i) = b(i)
        }

      }
    }
  }

  implicit def impl_OpSet_V_S_InPlace[V]: OpSet.InPlaceImpl2[Vector[V], V] = {
    new OpSet.InPlaceImpl2[Vector[V], V] {
      def apply(a: Vector[V], b: V): Unit = {
        for (i <- 0 until a.length) {
          a(i) = b
        }
      }
    }
  }

  // TODO: handle overlaps
  implicit def impl_scaleAdd_InPlace_V_T_V_Generic[T: Semiring]: scaleAdd.InPlaceImpl3[Vector[T], T, Vector[T]] = {
    (a: Vector[T], s: T, b: Vector[T]) => {
      val sr = implicitly[Semiring[T]]
      require(b.length == a.length, "Vectors must be the same length!")
      if (s != 0) {
        var i = 0
        for ((k, v) <- b.activeIterator) {
          a(k) = sr.+(a(k), sr.*(s, v))
          i += 1
        }
      }
    }
  }
}
