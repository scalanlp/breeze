package breeze.linalg.operators

import breeze.collection.mutable.OpenAddressHashArray
import breeze.generic.UFunc
import breeze.generic.UFunc.UImpl2
import breeze.linalg.support.{CanCopy, CanZipMapKeyValues, CanZipMapValues}
import breeze.linalg.{Vector, _}
import breeze.macros.expand
import breeze.math.{Field, Ring, Semiring}
import breeze.storage.Zero
import scalaxy.debug._
import breeze.macros._
import breeze.math.PowImplicits._

import scala.reflect.ClassTag
import scala.{specialized => spec}

trait DenseVector_HashVector_Ops extends GenericOps {

  @expand
  implicit def dv_hv_UpdateOp[
    @expand.args(Int, Double, Float, Long) T,
    @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
    ( implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
  op: Op.Impl1[T, T, T]): Op.InPlaceImpl2[DenseVector[T], HashVector[T]] =
    new Op.InPlaceImpl2[DenseVector[T], HashVector[T]] {
      def apply(a: DenseVector[T], b: HashVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")
        val ad = a.data
        var aoff = a.offset
        val astride = a.stride

        var i = 0
        while (i < a.length) {
          ad(aoff) = op(ad(aoff), b(i))
          aoff += astride
          i += 1
        }

      }
    }

//  // this shouldn't be necessary but it is:
//  @expand
//  implicit def dv_hv_op[
//    @expand.args(Int, Double, Float, Long) T,
//    @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
//  : UFunc.UImpl2[Op, DenseVector[T], HashVector[T], DenseVector[T]]= {
//    pureFromUpdate(implicitly[Op.InPlaceImpl2[DenseVector[T], HashVector[T]]])
//  }
  @expand
  @expand.valify
  implicit def dv_hv_ScaleAddInto[
    @expand.args(Int, Double, Float, Long) T]: scaleAdd.InPlaceImpl3[DenseVector[T], T, HashVector[T]] = {
    (dv, scalar, hv) =>
      require(dv.length == hv.length, "Vectors must have the same length")
      val ad = dv.data
      val bd = hv.data
      val bi = hv.index
      val bsize = hv.iterableSize

      cforRange(0 until bsize) { i =>
        val aoff = dv.offset + bi(i) * dv.stride
        if (hv.isActive(i))
          ad(aoff) += scalar * bd(i)
      }
  }

  @expand
  @expand.valify
  implicit def canDot_DV_HV[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0f, 0L) zero: T)
    : breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], HashVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], HashVector[T], T] {
      def apply(a: DenseVector[T], b: HashVector[T]) = {
        var result: T = zero

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        val adata = a.data
        val aoff = a.offset
        val stride = a.stride

        var i = 0
        while (i < bsize) {
          if (b.isActive(i))
            result += adata(aoff + bi(i) * stride) * bd(i)
          i += 1
        }
        result
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }
  }

}

trait HashVector_DenseVector_Ops extends DenseVector_HashVector_Ops {
  @expand
  @expand.valify
  implicit def hv_dv_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], DenseVector[T]] =
    new Op.InPlaceImpl2[HashVector[T], DenseVector[T]] {
      def apply(a: HashVector[T], b: DenseVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")

        var i = 0
        while (i < b.length) {
          a(i) = op(a(i), b(i))
          i += 1
        }
      }
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }
  @expand
  @expand.valify
  implicit def hv_dv_op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): Op.Impl2[HashVector[T], DenseVector[T], DenseVector[T]] = {
    new Op.Impl2[HashVector[T], DenseVector[T], DenseVector[T]] {
      def apply(a: HashVector[T], b: DenseVector[T]) = {
        require(a.length == b.length, "Vectors must have the same length")
        val result = DenseVector.zeros[T](a.length)

        var i = 0
        while (i < b.length) {
          result(i) = op(a(i), b(i))
          i += 1
        }

        result
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
  }

  @expand
  implicit def canDot_HV_DV[@expand.args(Int, Float, Double, Long) T]
    : breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], DenseVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], DenseVector[T], T] {
      def apply(a: HashVector[T], b: DenseVector[T]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        b.dot(a)
      }
    }
    //      Vector.canDotProductV_T.register(this)
  }

}

trait HashVectorOps extends HashVector_GenericOps {
  @expand
  @expand.valify
  implicit def hv_hv_RHS_Idempotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
      op: Op.Impl2[T, T, T]): Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] =
    new Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: HashVector[T]): HashVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")

        // if we're adding, we can do it the other way
        // upcast to prevent warning when Op = OpSub
        if ((Op: Any) == OpAdd && a.activeSize < b.activeSize) {
          return apply(b, a)
        }

        val result = a.copy
        for ((k, v) <- b.activeIterator) {
          result(k) = op(a(k), v)
        }
        result
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
  @expand
  @expand.valify
  implicit def hv_hv_nilpotent_Op[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T)
    : OpMulScalar.Impl2[HashVector[T], HashVector[T], HashVector[T]] =
    new OpMulScalar.Impl2[HashVector[T], HashVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: HashVector[T]): HashVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")

        // this op has the property that if either lhs or rhs is 0, then the result is 0
        if (a.activeSize < b.activeSize) return apply(b, a)

        val builder = new VectorBuilder[T](a.length)
        for ((k, v) <- b.activeIterator) {
          val r = a(k) * v
          if (r != zero)
            builder.add(k, r)
        }
        builder.toHashVector
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def hv_hv_LHS_Nilpotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpDiv, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] =
    new Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: HashVector[T]): HashVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")

        val result =
          new HashVector[T](new OpenAddressHashArray[T](a.length, default = 0, initialSize = a.array.iterableSize))

        if (b.activeSize != b.size) {
          // have 0s in RHS, will produce non-zero results with LHS 0s (NaN or throw)
          for ((k, v) <- a.iterator) {
            result(k) = op(v, b(k))
          }
        } else {
          for ((k, v) <- a.activeIterator) {
            result(k) = op(v, b(k))
          }
        }

        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
  @expand
  @expand.valify
  implicit def hv_v_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): Op.Impl2[HashVector[T], Vector[T], HashVector[T]] =
    new Op.Impl2[HashVector[T], Vector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: Vector[T]): HashVector[T] = {

        require(b.length == a.length, "Vectors must be the same length!")
        val result = HashVector.zeros[T](a.length)

        var i = 0
        while (i < a.length) {
          result(i) = op(a(i), b(i))
          i += 1
        }
        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def hv_s_rhs_idempotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
      op: Op.Impl2[T, T, T],
      @expand.sequence[T](0, 0.0, 0.0f, 0L)
      zero: T): Op.Impl2[HashVector[T], T, HashVector[T]] = new Op.Impl2[HashVector[T], T, HashVector[T]] {
    def apply(a: HashVector[T], b: T): HashVector[T] = {

      if (b == 0) {
        return a.copy
      }

      val result = HashVector.zeros[T](a.length)
      var i = 0
      while (i < a.length) {
        result(i) = op(a(i), b)
        i += 1
      }
      result
    }
    implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def hv_s_LHS_Nilpotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ * _ }, { _ / _ }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T],
      @expand.sequence[T](0, 0.0, 0.0f, 0L)
      zero: T): Op.Impl2[HashVector[T], T, HashVector[T]] = new Op.Impl2[HashVector[T], T, HashVector[T]] {
    def apply(a: HashVector[T], b: T): HashVector[T] = {
      val result = HashVector.zeros[T](a.length)

      // can short-circuit multiplication by 0
      // upcast to prevent warning
      if (((Op: Any) == OpMulScalar || (Op: Any) == OpMulMatrix) && b == 0) {
        return result
      }

      if (b == 0) { // in a degenerate case, need to iterate all
        for ((k, v) <- a.iterator) {
          result(k) = op(v, b)
        }
      } else {
        for ((k, v) <- a.activeIterator) {
          result(k) = op(v, b)
        }
      }

      result
    }
    implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def hv_hv_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], HashVector[T]] =
    new Op.InPlaceImpl2[HashVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: HashVector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        var i = 0
        while (i < a.length) {
          a(i) = op(a(i), b(i))
          i += 1
        }
      }
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def hv_hv_Set[@expand.args(Int, Double, Float, Long) T]: OpSet.InPlaceImpl2[HashVector[T], HashVector[T]] =
    new OpSet.InPlaceImpl2[HashVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: HashVector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        b.array.copyTo(a.array)
      }
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], OpSet.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def hv_hv_Idempotent_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], HashVector[T]] =
    new Op.InPlaceImpl2[HashVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: HashVector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        for ((k, v) <- b.activeIterator) {
          a(k) = op(a(k), v)
        }
      }

      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def hv_s_RHS_idempotent_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], T] = new Op.InPlaceImpl2[HashVector[T], T] {
    def apply(a: HashVector[T], b: T): Unit = {
      if (b == 0) return

      var i = 0
      while (i < a.length) {
        a(i) = op(a(i), b)
        i += 1
      }
    }
    implicitly[BinaryUpdateRegistry[Vector[T], T, Op.type]].register(this)
  }

  @expand
  @expand.valify
  implicit def hv_s_nilpotent_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ })
      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], T] = new Op.InPlaceImpl2[HashVector[T], T] {
    def apply(a: HashVector[T], b: T): Unit = {
      if (b == 0) {
        a.clear()
        return
      }

      for ((k, v) <- a.activeIterator) {
        a(k) = op(v, b)
      }
    }
    implicitly[BinaryUpdateRegistry[Vector[T], T, Op.type]].register(this)
  }

  @expand
  @expand.valify
  implicit def hv_s_SetOp[@expand.args(Int, Double, Float, Long) T]: OpSet.InPlaceImpl2[HashVector[T], T] =
    new OpSet.InPlaceImpl2[HashVector[T], T] {
      def apply(a: HashVector[T], b: T): Unit = {
        if (b == 0) {
          a.clear()
          return
        }

        var i = 0
        while (i < a.length) {
          a(i) = b
          i += 1
        }
      }
      implicitly[BinaryUpdateRegistry[Vector[T], T, OpSet.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def hv_s_LHS_nilpotent_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpDiv, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], T] = new Op.InPlaceImpl2[HashVector[T], T] {
    def apply(a: HashVector[T], b: T): Unit = {
      if (b == 0) {
        // scalar 0 does bad things with these ops
        var i = 0
        while (i < a.length) {
          a(i) = op(a(i), b)
          i += 1
        }
      } else {
        for ((k, v) <- a.activeIterator) {
          a(k) = op(v, b)
        }
      }
    }
    implicitly[BinaryUpdateRegistry[Vector[T], T, Op.type]].register(this)
  }

  @expand
  @expand.valify
  implicit def canDot_HV_HV[@expand.args(Int, Long, Double, Float) T](
      implicit @expand.sequence[T](0, 0L, 0.0, 0f) zero: T)
    : breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], HashVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], HashVector[T], T] {
      def apply(a: HashVector[T], b: HashVector[T]): T = {
        require(b.length == a.length, "Vectors must be the same length!")

        if (a.iterableSize > b.iterableSize) {
          apply(b, a)
        } else {
          var result: T = zero
          for ((k, v) <- a.activeIterator) {
            result += v * b(k)
          }
          result
        }
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }
  }

  @expand
  implicit def canNorm[@expand.args(Int, Double, Float, Long) T]: norm.Impl2[HashVector[T], Double, Double] = {

    new norm.Impl2[HashVector[T], Double, Double] {
      def apply(v: HashVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          activeValuesIterator.foreach(v => sum += v.abs.toDouble)
          sum
        } else if (n == 2) {
          var sum = 0.0
          activeValuesIterator.foreach(v => { val nn = v.abs.toDouble; sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          activeValuesIterator.foreach(v => { val nn = v.abs.toDouble; if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          activeValuesIterator.foreach(v => { val nn = v.abs.toDouble; sum += math.pow(nn, n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

  implicit def canNorm[T: Field: ClassTag]: norm.Impl2[HashVector[T], Double, Double] = {

    new norm.Impl2[HashVector[T], Double, Double] {
      val f: Field[T] = implicitly[Field[T]]
      def apply(v: HashVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          activeValuesIterator.foreach(v => sum += f.sNorm(v))
          sum
        } else if (n == 2) {
          var sum = 0.0
          activeValuesIterator.foreach(v => { val nn = f.sNorm(v); sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          activeValuesIterator.foreach(v => { val nn = f.sNorm(v); if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          activeValuesIterator.foreach(v => { val nn = f.sNorm(v); sum += math.pow(nn, n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

}

trait HashVector_SparseVector_Ops extends HashVectorOps {

  @expand
  @expand.valify
  implicit def hv_sv_lhs_nilpotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T],
      @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T): Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] =
    new Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: SparseVector[T]): HashVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val builder = new VectorBuilder[T](a.length)
        if ((Op: Any) == OpSet || b.activeSize != b.length) {
          cforRange(0 until b.length) { k =>
            val r = op(a(k), b.otherApply(k))
            if (r != zero)
              builder.add(k, r)
          }
        } else {
          for ((k, v) <- a.activeIterator) {
            val r = op(v, b.otherApply(k))
            if (r != zero)
              builder.add(k, r)
          }
        }
        builder.toHashVector
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
  @expand
  @expand.valify
  implicit def hv_sv_nilpotent_Op[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T)
    : OpMulScalar.Impl2[HashVector[T], SparseVector[T], HashVector[T]] =
    new OpMulScalar.Impl2[HashVector[T], SparseVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: SparseVector[T]): HashVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        // TODO: if a is enough shorter than b, we should loop over it instead
        val builder = new VectorBuilder[T](a.length)
        cforRange(0 until b.activeSize) { boff =>
          val i = b.indexAt(boff)
          val v = b.valueAt(boff)
          val r = a(i) * v

          if (r != zero)
            builder.add(i, r)
        }
        builder.toHashVector
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]]].register(this)
    }

  @expand.valify
  @expand
  implicit def hv_sv_UpdateOp[@expand.args(Int, Double, Float, Long) T, @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }) op: Op.Impl2[T, T, T]
  ): Op.InPlaceImpl2[HashVector[T], SparseVector[T]] = {
    new Op.InPlaceImpl2[HashVector[T], SparseVector[T]] {
      def apply(a: HashVector[T], b: SparseVector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        var boff = 0

        while (boff < b.activeSize) {
          val k = b.indexAt(boff)
          val v = b.valueAt(boff)
          a(k) = op(a(k), v)
          boff += 1
        }
      }
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def canDot_HV_SV[@expand.args(Int, Long, Float, Double) T](
      implicit @expand.sequence[T](0, 0L, 0f, 0.0) zero: T)
    : breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], SparseVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], SparseVector[T], T] {
      def apply(a: HashVector[T], b: SparseVector[T]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        var result: T = zero

        // TODO: if a has much less nnz then b, then it would make sense to do a instead.
        // but iterating over b is faster than iterating over a and indexing into a is faster than indexing into b
        var boff = 0

        while (boff < b.activeSize) {
          result += a(b.indexAt(boff)) * b.valueAt(boff)
          boff += 1
        }

        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }
  }
}

trait SparseVector_HashVector_Ops extends HashVectorOps with HashVector_SparseVector_Ops {
  @expand.valify
  @expand
  implicit def sv_hv_lhs_nilpotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T],
      @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T): Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] =
    new Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: HashVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val builder = new VectorBuilder[T](a.length)
        if ((Op: Any) == OpSet || b.activeSize != b.length) {
          cforRange(0 until a.length) { k =>
            val r: T = op(a.otherApply(k), b(k))
            if (r != zero)
              builder.add(k, r)
          }
        } else {
          var aoff = 0
          while (aoff < a.activeSize) {
            val k = a.indexAt(aoff)
            val v = a.valueAt(aoff)
            val r = op(v, b(k))
            if (r != zero)
              builder.add(k, r)
            aoff += 1
          }
        }
        builder.toSparseVector(alreadySorted = true, keysAlreadyUnique = true)
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  @expand.valify
  @expand
  implicit def sv_hv_nilpotent_Op[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T)
    : OpMulScalar.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] =
    new OpMulScalar.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: HashVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val builder = new VectorBuilder[T](a.length)
        var aoff = 0
        while (aoff < a.activeSize) {
          val k = a.indexAt(aoff)
          val v = a.valueAt(aoff)
          val r = v * b(k)
          if (r != zero)
            builder.add(k, r)
          aoff += 1
        }
        builder.toSparseVector(alreadySorted = true, keysAlreadyUnique = true)
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]]].register(this)
    }
  @expand
  @expand.valify
  implicit def sv_hv_Idempotent_Op[@expand.args(Int, Double, Float, Long) T, @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
      op: Op.Impl2[T, T, T]): Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] =
    new Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: HashVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val builder = new VectorBuilder[T](a.length)
        var aoff = 0
        while (aoff < a.activeSize) {
          val k = a.indexAt(aoff)
          val v = a.valueAt(aoff)
          builder.add(k, v)
          aoff += 1
        }

        for ((k, v) <- b.activeIterator) {
          builder.add(k, v)
        }

        builder.toSparseVector
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  implicit def canDot_SV_HV[T](implicit op: OpMulInner.Impl2[HashVector[T], SparseVector[T], T])
    : breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], HashVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], HashVector[T], T] {
      def apply(a: SparseVector[T], b: HashVector[T]) = {
        b.dot(a)
      }
    }
  }

  protected def updateFromPureS[T, Op, Other](
      implicit op: UFunc.UImpl2[Op, SparseVector[T], Other, SparseVector[T]],
      set: OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]]): UFunc.InPlaceImpl2[Op, SparseVector[T], Other] = {
    new UFunc.InPlaceImpl2[Op, SparseVector[T], Other] {
      def apply(a: SparseVector[T], b: Other): Unit = {
        val result = op(a, b)
        a := result
      }
    }
  }

  @expand.valify
  @expand
  implicit def sv_hv_update[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
    : Op.InPlaceImpl2[SparseVector[T], HashVector[T]] = {
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(updateFromPureS)
  }
}

trait HashVector_GenericOps {

  implicit def canSet_HV_Generic[V]: OpSet.InPlaceImpl2[HashVector[V], V] = {
    new OpSet.InPlaceImpl2[HashVector[V], V] {
      def apply(a: HashVector[V], b: V): Unit = {
        var i = 0
        while (i < a.length) {
          a(i) = b
          i += 1
        }

      }
    }
  }

  implicit def canSet_HV_HV_Generic[V]: OpSet.InPlaceImpl2[HashVector[V], Vector[V]] = {
    new OpSet.InPlaceImpl2[HashVector[V], Vector[V]] {
      def apply(a: HashVector[V], b: Vector[V]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        a.clear()
        for ((k, v) <- b.activeIterator) {
          a(k) = v
        }
      }
    }
  }

  implicit def HV_canGaxpy[V: Semiring]: scaleAdd.InPlaceImpl3[HashVector[V], V, HashVector[V]] = {
    new scaleAdd.InPlaceImpl3[HashVector[V], V, HashVector[V]] {
      val ring: Semiring[V] = implicitly[Semiring[V]]
      def apply(a: HashVector[V], s: V, b: HashVector[V]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")

        for ((k, v) <- b.activeIterator)
          a(k) = ring.+(a(k), ring.*(s, v))
      }
    }
  }

  class CanZipMapValuesHashVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV: ClassTag: Zero]
      extends CanZipMapValues[HashVector[V], V, RV, HashVector[RV]] {

    def create(length: Int) = HashVector.zeros(length)

    /**Maps all corresponding values from the two collections. */
    def map(from: HashVector[V], from2: HashVector[V], fn: (V, V) => RV) = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }

    def mapActive(from: HashVector[V], from2: HashVector[V], fn: (V, V) => RV) = {
      map(from, from2, fn)
    }

  }
  implicit def HV_zipMap[V, R: ClassTag: Zero]: CanZipMapValuesHashVector[V, R] = new CanZipMapValuesHashVector[V, R]
  implicit val HV_zipMap_d: CanZipMapValuesHashVector[Double, Double] = new CanZipMapValuesHashVector[Double, Double]
  implicit val HV_zipMap_f: CanZipMapValuesHashVector[Float, Float] = new CanZipMapValuesHashVector[Float, Float]
  implicit val HV_zipMap_i: CanZipMapValuesHashVector[Int, Int] = new CanZipMapValuesHashVector[Int, Int]

  class CanZipMapKeyValuesHashVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV: ClassTag: Zero]
      extends CanZipMapKeyValues[HashVector[V], Int, V, RV, HashVector[RV]] {

    def create(length: Int) = HashVector.zeros(length)

    /**Maps all corresponding values from the two collections. */
    def map(from: HashVector[V], from2: HashVector[V], fn: (Int, V, V) => RV) = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result(i) = fn(i, from(i), from2(i))
        i += 1
      }
      result
    }

    override def mapActive(from: HashVector[V], from2: HashVector[V], fn: (Int, V, V) => RV): HashVector[RV] = {
      map(from, from2, fn)
    }
  }
  implicit def HV_zipMapKV[V, R: ClassTag: Zero]: CanZipMapKeyValuesHashVector[V, R] = new CanZipMapKeyValuesHashVector[V, R]

  implicit def HV_addIntoField[T](
      implicit field: Field[T],
      ct: ClassTag[T]): OpAdd.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpAdd.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for (i <- 0 until v.length) v(i) = field.+(v(i), v2(i))
      }
    }

  }

  implicit def HV_subIntoField[T](
      implicit field: Field[T],
      ct: ClassTag[T]): OpSub.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpSub.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for (i <- 0 until v.length) v(i) = field.-(v(i), v2(i))
      }
    }

  }

  implicit def HV_mulIntoField[T](
      implicit field: Field[T],
      ct: ClassTag[T]): OpMulScalar.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpMulScalar.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for (i <- 0 until v.length) v(i) = field.*(v(i), v2(i))
      }
    }

  }

  implicit def HV_divIntoField[T](
      implicit field: Field[T],
      ct: ClassTag[T]): OpDiv.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpDiv.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for (i <- 0 until v.length) v(i) = field./(v(i), v2(i))
      }
    }

  }

  implicit def HV_addIntoField[T](implicit field: Semiring[T], ct: ClassTag[T]): OpAdd.InPlaceImpl2[HashVector[T], T] = {
    new OpAdd.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field.+(v(i), v2)
      }
    }

  }

//  implicit def hv_addSField[T](
//      implicit field: Semiring[T],
//      ct: ClassTag[T]): OpAdd.Impl2[HashVector[T], T, HashVector[T]] = {
//    binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vAddIntoSField, ct)
//  }
//  implicit def vSubSField[T](implicit field: Ring[T], ct: ClassTag[T]): OpSub.Impl2[HashVector[T], T, HashVector[T]] =
//    binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vSubIntoSField, ct)
//  implicit def vMulScalarSField[T](
//      implicit field: Semiring[T],
//      ct: ClassTag[T]): OpMulScalar.Impl2[HashVector[T], T, HashVector[T]] =
//    binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vMulScalarIntoSField, ct)
//  implicit def vDivSField[T](implicit field: Field[T], ct: ClassTag[T]): OpDiv.Impl2[HashVector[T], T, HashVector[T]] =
//    binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vDivIntoSField, ct)
//  implicit def vPowS[T](
//      implicit pow: OpPow.Impl2[T, T, T],
//      ct: ClassTag[T],
//      zero: Zero[T]): OpPow.Impl2[HashVector[T], T, HashVector[T]] =
//    binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vPowIntoS, ct)

  implicit def HV_S_subIntoField[T](implicit field: Ring[T], ct: ClassTag[T]): OpSub.InPlaceImpl2[HashVector[T], T] = {
    new OpSub.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        if (v2 == field.zero)
          return

        for (i <- 0 until v.length) v(i) = field.-(v(i), v2)
      }
    }

  }

  implicit def HV_S_mulScalarIntoField[T](
      implicit field: Semiring[T],
      ct: ClassTag[T]): OpMulScalar.InPlaceImpl2[HashVector[T], T] = {
    new OpMulScalar.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        if (v2 == field.zero) {
          v := field.zero
        } else if (v2 == field.one) {
          return
        } else {
          for (i <- 0 until v.length) v(i) = field.*(v(i), v2)
        }
      }
    }
  }

  implicit def vDivIntoSField[T](implicit field: Field[T], ct: ClassTag[T]): OpDiv.InPlaceImpl2[HashVector[T], T] = {
    new OpDiv.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field./(v(i), v2)
      }
    }
  }

  implicit def vPowIntoS[T](
      implicit pow: OpPow.Impl2[T, T, T],
      ct: ClassTag[T]): OpPow.InPlaceImpl2[HashVector[T], T] = {
    new OpPow.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = pow(v(i), v2)
      }
    }
  }

  // TODO: try removing
  implicit def HV_dotField[T](implicit field: Semiring[T]): OpMulInner.Impl2[HashVector[T], HashVector[T], T] = {
    new OpMulInner.Impl2[HashVector[T], HashVector[T], T] {
      override def apply(v: HashVector[T], v2: HashVector[T]): T = {
        var acc = field.zero
        for (i <- 0 until v.length) {
          acc = field.+(acc, field.*(v(i), v2(i)))
        }
        acc
      }
    }
  }

  def binaryOpFromUpdateOp[Op <: OpType, V, Other](
      implicit copy: CanCopy[HashVector[V]],
      op: UFunc.InPlaceImpl2[Op, HashVector[V], Other],
      man: ClassTag[V]): UFunc.UImpl2[Op, HashVector[V], Other, HashVector[V]] = {
    new UFunc.UImpl2[Op, HashVector[V], Other, HashVector[V]] {
      override def apply(a: HashVector[V], b: Other): HashVector[V] = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def implOpSet_V_V_InPlace[V]: OpSet.InPlaceImpl2[HashVector[V], HashVector[V]] = {

    new OpSet.InPlaceImpl2[HashVector[V], HashVector[V]] {
      def apply(a: HashVector[V], b: HashVector[V]): Unit = {
        require(b.length == a.length, "HashVectors must be the same length!")
        for (i <- 0 until a.length) {
          a(i) = b(i)
        }
      }
    }
  }

  /**Returns the k-norm of this HashVector. */
  implicit def canNorm[T](implicit canNormS: norm.Impl[T, Double]): norm.Impl2[HashVector[T], Double, Double] = {

    new norm.Impl2[HashVector[T], Double, Double] {
      def apply(v: HashVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          activeValuesIterator.foreach(v => sum += canNormS(v))
          sum
        } else if (n == 2) {
          var sum = 0.0
          activeValuesIterator.foreach(v => { val nn = canNormS(v); sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          activeValuesIterator.foreach(v => { val nn = canNormS(v); if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          activeValuesIterator.foreach(v => { val nn = canNormS(v); sum += math.pow(nn, n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

}
