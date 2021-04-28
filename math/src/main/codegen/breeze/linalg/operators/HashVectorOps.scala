package breeze.linalg.operators

import breeze.collection.mutable.OpenAddressHashArray
import breeze.generic.UFunc
import breeze.generic.UFunc.UImpl2
import breeze.linalg.support.{CanCopy, CanTraverseValues, CanZipMapKeyValues, CanZipMapValues}
import breeze.linalg._
import breeze.macros.expand
import breeze.math.{Field, Ring, Semiring}
import breeze.storage.Zero
import scalaxy.debug._
import breeze.macros._
import breeze.math.PowImplicits._

import scala.reflect.ClassTag
import scala.{specialized => spec}

trait HashVectorOps
    extends HashVectorExpandOps
    with DenseVector_HashVector_Ops
    with HashVector_DenseVector_Ops
    with HashVector_SparseVector_Ops
    with SparseVector_HashVector_Ops

trait DenseVector_HashVector_Ops extends GenericOps with DenseVectorOps with HashVectorExpandOps {

  @expand
  implicit def impl_Op_InPlace_DV_HV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, { (__x, __y) =>
        __y
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl1[T, T, T]): Op.InPlaceImpl2[DenseVector[T], HashVector[T]] =
    new Op.InPlaceImpl2[DenseVector[T], HashVector[T]] {
      def apply(a: DenseVector[T], b: HashVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")
        val ad = a.data
        var aoff = a.offset
        val astride = a.stride

        // TODO: replace OpMulScalar with faster variant for common cases?
        cforRange(0 until a.length) { i =>
          ad(aoff) = op(ad(aoff), b(i))
          aoff += astride
        }

      }
    }

  @expand
  @expand.valify
  implicit def impl_scaleAdd_InPlace_DV_T_HV[@expand.args(Int, Double, Float, Long) T]
    : scaleAdd.InPlaceImpl3[DenseVector[T], T, HashVector[T]] = { (dv, scalar, hv) =>
    require(dv.length == hv.length, "Vectors must have the same length")
    val ad = dv.data
    val bd = hv.data
    val bi = hv.index
    val bsize = hv.iterableSize

    if (scalar != 0)
      cforRange(0 until bsize) { i =>
        val aoff = dv.offset + bi(i) * dv.stride
        if (hv.isActive(i))
          ad(aoff) += scalar * bd(i)
      }
  }

  @expand
  @expand.valify
  implicit def impl_OpMulInner_DV_HV_eq_S[@expand.args(Int, Double, Float, Long) T](
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

        cforRange(0 until bsize) { i =>
          if (b.isActive(i))
            result += adata(aoff + bi(i) * stride) * bd(i)
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
  implicit def impl_Op_InPlace_HV_DV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, { (__x, __y) =>
        __y
      }, { _ % _ }, { _.pow(_) })
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
  implicit def impl_Op_HV_DV_eq_HV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, { (__x, __y) =>
        __y
      }, { _ % _ }, { _.pow(_) })
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
  implicit def impl_OpMulInner_HV_DV_eq_T[@expand.args(Int, Float, Double, Long) T]
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

trait HashVectorExpandOps extends VectorOps with HashVector_GenericOps {
  @expand
  @expand.valify
  implicit def impl_scaleAdd_InPlace_HV_HV_HV[@expand.args(Int, Double, Float, Long) T]
  : scaleAdd.InPlaceImpl3[HashVector[T], T, HashVector[T]] = { (dest, scalar, source) =>
    require(dest.length == source.length, "Vectors must have the same length")
    val bsize = source.iterableSize

    if (scalar != 0) {
      val bd = source.data
      val bi = source.index
      cforRange(0 until bsize) { i =>
        if (source.isActive(i))
          dest(bi(i)) += scalar * bd(i)
      }
    }
  }

  @expand
  @expand.valify
  implicit def impl_Op_HV_HV_eq_HV[
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
  implicit def impl_OpMulScalar_HV_HV_eq_HV[@expand.args(Int, Double, Float, Long) T](
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
  implicit def impl_Op_HV_HV_eq_HV_lhs_nilpotent[
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
  implicit def impl_Op_HV_V_eq_HV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, { (__x, __y) =>
        __y
      }, { _ % _ }, { _.pow(_) })
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
  implicit def impl_Op_HV_S_eq_HV_add[
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
  implicit def impl_Op_HV_S_eq_HV_zeroy[
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

//  @expand
//  @expand.valify
//  implicit def hv_hv_UpdateOp[
//      @expand.args(Int, Double, Float, Long) T,
//      @expand.args(OpMulScalar, OpDiv, OpMod, OpPow) Op <: OpType](
//      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, { _ % _ }, { _.pow(_) })
//      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], HashVector[T]] =
//    new Op.InPlaceImpl2[HashVector[T], HashVector[T]] {
//      def apply(a: HashVector[T], b: HashVector[T]): Unit = {
//        require(b.length == a.length, "Vectors must be the same length!")
//        var i = 0
//        while (i < a.length) {
//          a(i) = op(a(i), b(i))
//          i += 1
//        }
//      }
//      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
//    }

  @expand
  @expand.valify
  implicit def impl_OpSet_InPlace_HV_HV[@expand.args(Int, Double, Float, Long) T]: OpSet.InPlaceImpl2[HashVector[T], HashVector[T]] =
    new OpSet.InPlaceImpl2[HashVector[T], HashVector[T]] {
      def apply(a: HashVector[T], b: HashVector[T]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        b.array.copyTo(a.array)
      }
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], OpSet.type]].register(this)
    }

//  @expand
//  @expand.valify
//  implicit def impl_Op_InPlace_HV_HV_idempotent[
//      @expand.args(Int, Double, Float, Long) T,
//      @expand.args(OpAdd, OpSub) Op <: OpType](
//      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ })
//      op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[HashVector[T], HashVector[T]] =
//    new Op.InPlaceImpl2[HashVector[T], HashVector[T]] {
//      def apply(a: HashVector[T], b: HashVector[T]): Unit = {
//        require(b.length == a.length, "Vectors must be the same length!")
//        for ((k, v) <- b.activeIterator) {
//          a(k) = op(a(k), v)
//        }
//      }
//
//      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
//    }

  @expand
  @expand.valify
  implicit def impl_Op_InPlace_HV_S_idempotent[
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
  implicit def impl_OpMulScalar_InPlace_HV_S[@expand.args(Int, Double, Float, Long) T]
  : OpMulScalar.InPlaceImpl2[HashVector[T], T] = new OpMulScalar.InPlaceImpl2[HashVector[T], T] {
    def apply(a: HashVector[T], b: T): Unit = {
      if (b == 0) {
        a.clear()
        return
      }

      for ((k, v) <- a.activeIterator) {
        a(k) = v * b
      }
    }
    implicitly[BinaryUpdateRegistry[Vector[T], T, OpMulScalar.type]].register(this)
  }

  @expand
  @expand.valify
  implicit def impl_OpSet_InPlace_HV_S[@expand.args(Int, Double, Float, Long) T]: OpSet.InPlaceImpl2[HashVector[T], T] =
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
  implicit def impl_Op_InPlace_HV_S_LHS_nilpotent[
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
  implicit def impl_OpMulInner_HV_HV_eq_S[@expand.args(Int, Long, Double, Float) T](
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

  // TODO(perf): do we need these specialized ones?
  @expand
  @expand.valify
  implicit def impl_CanTraverseValues_HV[@expand.args(Int, Double, Float, Long) T]: CanTraverseValues[HashVector[T], T] = {
    new CanTraverseValues[HashVector[T], T] {
      /**   Traverses all values from the given collection. */
      override def traverse(from: HashVector[T], fn: CanTraverseValues.ValuesVisitor[T]): Unit = {
        cforRange(0 until from.iterableSize) { i =>
          if (from.isActive(i)) {
            fn.visit(from.data(i))
          }
        }

        fn.zeros(from.size - from.activeSize, 0)
      }

      override def isTraversableAgain(from: HashVector[T]): Boolean = true
    }
  }

  implicit def impl_CanTraverseValues_HV_Generic[T]: CanTraverseValues[HashVector[T], T] = {
    new CanTraverseValues[HashVector[T], T] {
      /**   Traverses all values from the given collection. */
      override def traverse(from: HashVector[T], fn: CanTraverseValues.ValuesVisitor[T]): Unit = {
        cforRange(0 until from.iterableSize) { i =>
          if (from.isActive(i)) {
            fn.visit(from.data(i))
          }
        }

        fn.zeros(from.size - from.activeSize, from.default)
      }

      override def isTraversableAgain(from: HashVector[T]): Boolean = true
    }
  }


  //  @expand
//  implicit def canNorm[@expand.args(Int, Double, Float, Long) T]: norm.Impl2[HashVector[T], Double, Double] = {
//
//    new norm.Impl2[HashVector[T], Double, Double] {
//      def apply(v: HashVector[T], n: Double): Double = {
//        import v._
//        if (n == 1) {
//          var sum = 0.0
//          activeValuesIterator.foreach(v => sum += v.abs.toDouble)
//          sum
//        } else if (n == 2) {
//          var sum = 0.0
//          activeValuesIterator.foreach(v => { val nn = v.abs.toDouble; sum += nn * nn })
//          math.sqrt(sum)
//        } else if (n == Double.PositiveInfinity) {
//          var max = 0.0
//          activeValuesIterator.foreach(v => { val nn = v.abs.toDouble; if (nn > max) max = nn })
//          max
//        } else {
//          var sum = 0.0
//          activeValuesIterator.foreach(v => { val nn = v.abs.toDouble; sum += math.pow(nn, n) })
//          math.pow(sum, 1.0 / n)
//        }
//      }
//    }
//  }
//
//  implicit def canNorm[T: Field: ClassTag]: norm.Impl2[HashVector[T], Double, Double] = {
//
//    new norm.Impl2[HashVector[T], Double, Double] {
//      val f: Field[T] = implicitly[Field[T]]
//      def apply(v: HashVector[T], n: Double): Double = {
//        import v._
//        if (n == 1) {
//          var sum = 0.0
//          activeValuesIterator.foreach(v => sum += f.sNorm(v))
//          sum
//        } else if (n == 2) {
//          var sum = 0.0
//          activeValuesIterator.foreach(v => { val nn = f.sNorm(v); sum += nn * nn })
//          math.sqrt(sum)
//        } else if (n == Double.PositiveInfinity) {
//          var max = 0.0
//          activeValuesIterator.foreach(v => { val nn = f.sNorm(v); if (nn > max) max = nn })
//          max
//        } else {
//          var sum = 0.0
//          activeValuesIterator.foreach(v => { val nn = f.sNorm(v); sum += math.pow(nn, n) })
//          math.pow(sum, 1.0 / n)
//        }
//      }
//    }
//  }

}

trait HashVector_SparseVector_Ops extends HashVectorExpandOps {

  @expand
  @expand.valify
  implicit def hv_sv_lhs_nilpotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ }, { (__x, __y) =>
        __y
      }, { _ % _ }, { _.pow(_) })
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

trait SparseVector_HashVector_Ops extends HashVectorExpandOps with HashVector_SparseVector_Ops with SparseVector_GenericOps {
  @expand.valify
  @expand
  implicit def sv_hv_lhs_nilpotent_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ / _ }, { (__x, __y) =>
        __y
      }, { _ % _ }, { _.pow(_) })
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

  @expand.valify
  @expand
  implicit def impl_Op_InPlace_SV_HV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
    : Op.InPlaceImpl2[SparseVector[T], HashVector[T]] = {
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]]
      .register(GenericOps.updateFromPure[Op.type, SparseVector[T], HashVector[T], SparseVector[T]])
  }
}
