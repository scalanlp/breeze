package breeze.linalg
package operators

import breeze.generic.UFunc
import breeze.linalg.support._
import breeze.macros.expand
import breeze.math._
import breeze.storage.Zero
import breeze.util.{ArrayUtil, ReflectionUtil}
import scalaxy.debug._
import breeze.macros._

import java.util
import scala.reflect.ClassTag
import scala.{specialized => spec}
import breeze.math.PowImplicits._

trait SparseVectorOps extends SparseVectorExpandOps
  with DenseVector_SparseVector_Ops
  with SparseVector_DenseMatrixOps
  with SparseVector_DenseVector_Ops

trait SparseVector_DenseVector_Ops extends VectorOps with GenericOps with SparseVector_TraversalOps {

  @expand
  @expand.valify
  implicit def impl_Op_SV_DV_InPlace[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](implicit @expand.sequence[Op](
    _ + _
  ,  _ - _,  _ * _, _ / _,  (__x, __y) => __y, _ % _, _.pow(_)) op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[SparseVector[T], DenseVector[T]] =
    new Op.InPlaceImpl2[SparseVector[T], DenseVector[T]] {

      def apply(a: SparseVector[T], b: DenseVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length, a.length)
        val bd: Array[T] = b.data
        val adefault: T = a.array.default
        var boff: Int = b.offset
        val asize: Int = a.activeSize
        val bstride: Int = b.stride
        val ad: Array[T] = a.data
        val ai: Array[Int] = a.index

        var i = 0
        var j = 0
        while (i < asize) {
          // do defaults until we get to the next aoffset
          val nextBoff: Int = b.offset + ai(i) * bstride
          while (boff < nextBoff) {
            result.add(j, op(adefault, bd(boff)))
            boff += bstride
            j += 1
          }

          result.add(j, op(ad(i), bd(boff)))
          boff += b.stride
          i += 1
          j += 1
        }

        while (boff < bd.length) {
          result.add(j, op(adefault, bd(boff)))
          boff += bstride
          j += 1
        }

        val rs: SparseVector[T] = result.toSparseVector(true, true)
        a.use(rs.index, rs.data, rs.activeSize)
      }
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_Op_SV_DV_eq_SV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }) op: Op.Impl2[T, T, T])
    : Op.Impl2[SparseVector[T], DenseVector[T], SparseVector[T]] = {

    new Op.Impl2[SparseVector[T], DenseVector[T], SparseVector[T]] {

      def apply(a: SparseVector[T], b: DenseVector[T]): SparseVector[T] = {

        require(a.length == b.length, "Vectors must have the same length")
        val result = VectorBuilder.zeros[T](a.length)
        val bd: Array[T] = b.data
        val adefault: T = a.array.default
        var boff: Int = b.offset
        val asize: Int = a.activeSize
        val bstride: Int = b.stride
        val ad: Array[T] = a.data
        val ai: Array[Int] = a.index

        cforRange(0 until a.activeSize) { i =>
          val ind = a.indexAt(i)
          val res: T = op(a.valueAt(i), b(ind))
          if (res != 0)
            result.add(ind, res)
        }

        result.toSparseVector(true, true)
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def impl_Op_SV_DV_eq_DV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) }) op: Op.Impl2[T, T, T]): Op.Impl2[SparseVector[T], DenseVector[T], DenseVector[T]] =
    new Op.Impl2[SparseVector[T], DenseVector[T], DenseVector[T]] {

      def apply(a: SparseVector[T], b: DenseVector[T]): DenseVector[T] = {

        require(a.length == b.length, "Vectors must have the same length")
        val result: DenseVector[T] = DenseVector.zeros[T](a.length)
        val bd: Array[T] = b.data
        val adefault: T = a.array.default
        var boff: Int = b.offset
        val asize: Int = a.activeSize
        val bstride: Int = b.stride
        val ad: Array[T] = a.data
        val ai: Array[Int] = a.index

        var i = 0
        var j = 0
        while (i < asize) {
          // do defaults until we get to the next aoffset
          val nextBoff: Int = b.offset + ai(i) * bstride
          while (boff < nextBoff) {
            result(j) = op(adefault, bd(boff))
            boff += bstride
            j += 1
          }

          result(j) = op(ad(i), bd(boff))
          boff += b.stride
          i += 1
          j += 1
        }

        while (boff < bd.length) {
          result(j) = op(adefault, bd(boff))
          boff += bstride
          j += 1
        }

        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
  @expand
  @expand.valify
  implicit def impl_OpMulInner_SV_DV_eq_T[@expand.args(Int, Double, Float, Long) T]
    : breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], DenseVector[T], T] =
    new breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], DenseVector[T], T] {
      def apply(a: SparseVector[T], b: DenseVector[T]): T = {
        require(b.length == a.length, "Vectors must be the same length!")
        b.dot(a)
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_SV_DVt_eq_SMT[@expand.args(Int, Double, Float, Long, Complex) T](
      implicit @expand.sequence[T](0, 0.0, 0.0f, 0L, Complex.zero) _zero: T)
    : OpMulMatrix.Impl2[SparseVector[T], Transpose[DenseVector[T]], CSCMatrix[T]] = {
    new OpMulMatrix.Impl2[SparseVector[T], Transpose[DenseVector[T]], CSCMatrix[T]] {
      def apply(a: SparseVector[T], b: Transpose[DenseVector[T]]): CSCMatrix[T] = {
        val sizeHint = a.activeSize * b.inner.size
        val res = new CSCMatrix.Builder[T](a.size, b.inner.size, sizeHint)

        for {(j, bValue) <- b.inner.activeIterator}
          for {(i, aValue) <- a.activeIterator}
            res.add(i, j, aValue * bValue)

        res.result(keysAlreadyUnique = true, keysAlreadySorted = true)
      }
    }
  }
}

trait DenseVector_SparseVector_Ops extends DenseVectorOps with SparseVectorExpandOps {
  @expand
  @expand.valify
  implicit def impl_Op_DV_SV_InPlace[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) }) op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[DenseVector[T], SparseVector[T]] =
    new Op.InPlaceImpl2[DenseVector[T], SparseVector[T]] {
      def apply(a: DenseVector[T], b: SparseVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")
        val ad: Array[T] = a.data
        val bdefault: T = b.array.default
        var aoff: Int = a.offset
        val bsize: Int = b.activeSize
        val astride: Int = a.stride
        val bd: Array[T] = b.data
        val bi: Array[Int] = b.index

        var i: Int = 0
        while (i < bsize) {
          // do defaults until we get to the next aoffset
          val nextAoff: Int = a.offset + bi(i) * astride
          while (aoff < nextAoff) {
            ad(aoff) = op(ad(aoff), bdefault)
            aoff += astride
          }

          ad(aoff) = op(ad(aoff), bd(i))
          aoff += a.stride
          i += 1
        }

        while (aoff < ad.length) {
          ad(aoff) = op(ad(aoff), bdefault)
          aoff += astride
        }
      }
      implicitly[BinaryUpdateRegistry[DenseVector[T], Vector[T], Op.type]].register(this)
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_Op_DV_SV_InPlace[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }) op: Op.Impl2[T, T, T])
    : Op.InPlaceImpl2[DenseVector[T], SparseVector[T]] =
    new Op.InPlaceImpl2[DenseVector[T], SparseVector[T]] {
      def apply(a: DenseVector[T], b: SparseVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")
        val ad: Array[T] = a.data
        val bd: Array[T] = b.data
        val bi: Array[Int] = b.index
        val bsize: Int = b.iterableSize

        var i: Int = 0
        while (i < bsize) {
          val aoff: Int = a.offset + bi(i) * a.stride
          ad(aoff) = op(ad(aoff), bd(i))
          i += 1
        }
      }
      implicitly[BinaryUpdateRegistry[DenseVector[T], Vector[T], Op.type]].register(this)
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_Op_DV_SV_eq_SV[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }) op: Op.Impl2[T, T, T])
    : Op.Impl2[DenseVector[T], SparseVector[T], SparseVector[T]] = {

    new Op.Impl2[DenseVector[T], SparseVector[T], SparseVector[T]] {

      def apply(a: DenseVector[T], b: SparseVector[T]): SparseVector[T] = {

        require(a.length == b.length, "Vectors must have the same length")
        val result = VectorBuilder.zeros[T](a.length)

        cforRange(0 until b.activeSize) { i =>
          val ind = b.indexAt(i)
          val res: T = op(a(ind), b.valueAt(i))
          if (res != 0)
            result.add(ind, res)
        }

        result.toSparseVector(true, true)
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  }

  @expand
  @expand.valify
  implicit def impl_OpMulInner_DV_SV_eq_T[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0.0f, 0L) _zero: T): OpMulInner.Impl2[DenseVector[T], SparseVector[T], T] =
    new OpMulInner.Impl2[DenseVector[T], SparseVector[T], T] {
      def apply(a: DenseVector[T], b: SparseVector[T]): T = {
        var result: T = _zero

        val bd: Array[T] = b.data
        val bi: Array[Int] = b.index
        val bsize: Int = b.iterableSize

        val adata: Array[T] = a.data
        val aoff: Int = a.offset
        val stride: Int = a.stride

        if (stride == 1) {
          cforRange (0 until bsize) { i =>
            result += adata(aoff + bi(i)) * bd(i)
          }
        } else {
          cforRange (0 until bsize) { i =>
            result += adata(aoff + bi(i) * stride) * bd(i)
          }
        }
        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
      //      implicitly[BinaryRegistry[DenseVector[T], Vector[T], OpMulInner.type, T]].register(this)
    }
  @expand
  @expand.valify
  implicit def impl_zipValues_DV_SV_eq_ZV[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T)
    : zipValues.Impl2[DenseVector[T], SparseVector[T], ZippedValues[T, T]] =
    new zipValues.Impl2[DenseVector[T], SparseVector[T], ZippedValues[T, T]] {

      def apply(du: DenseVector[T], sv: SparseVector[T]): ZippedValues[T, T] = {
        require(sv.length == du.length, "vector length mismatch")
        new ZippedValues[T, T] {
          def foreach(fn: (T, T) => Unit): Unit = {

            val n: Int = du.length

            val duData: Array[T] = du.data
            val duStride: Int = du.stride
            var duOffset: Int = du.offset

            val svIndices: Array[Int] = sv.index
            val svValues: Array[T] = sv.data
            val svActiveSize: Int = sv.activeSize

            var i: Int = 0
            var j: Int = 0
            while (j < svActiveSize) {
              val svIndex = svIndices(j)
              while (i < svIndex) {
                fn(duData(duOffset), zero)
                i += 1
                duOffset += duStride
              }
              fn(duData(duOffset), svValues(j))
              i += 1
              duOffset += duStride
              j += 1
            }
            while (i < n) {
              fn(duData(duOffset), zero)
              i += 1
              duOffset += duStride
            }
          }
        }

      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], zipValues.type, ZippedValues[T, T]]]

    }
  @expand
  @expand.valify
  implicit def implScaleAdd_DV_S_SV_InPlace[@expand.args(Int, Double, Float, Long) T](
      implicit @expand.sequence[T](0, 0.0, 0f, 0L) zero: T): scaleAdd.InPlaceImpl3[DenseVector[T], T, SparseVector[T]] =
    new scaleAdd.InPlaceImpl3[DenseVector[T], T, SparseVector[T]] {
      def apply(y: DenseVector[T], a: T, x: SparseVector[T]): Unit = {
        require(x.length == y.length, "Vectors must be the same length!")
        val xsize: Int = x.activeSize

        if (a != zero) {

          var xoff: Int = 0
          while (xoff < xsize) {
            y(x.indexAt(xoff)) += a * x.valueAt(xoff)
            xoff += 1
          }

        }

      }
      implicitly[TernaryUpdateRegistry[Vector[T], T, Vector[T], scaleAdd.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_DV_SVt_eq_CSC[@expand.args(Int, Double, Float, Long, Complex) T](
    implicit @expand.sequence[T](0, 0.0, 0.0f, 0L, Complex.zero) _zero: T)
    : OpMulMatrix.Impl2[DenseVector[T], Transpose[SparseVector[T]], CSCMatrix[T]] = {
    new OpMulMatrix.Impl2[DenseVector[T], Transpose[SparseVector[T]], CSCMatrix[T]] {
      def apply(a: DenseVector[T], b: Transpose[SparseVector[T]]): CSCMatrix[T] = {
        val bInner = b.inner
        val sizeHint = a.size * bInner.activeSize
        val res = new CSCMatrix.Builder[T](a.size, bInner.size, sizeHint)

        cforRange2(0 until bInner.activeSize, 0 until a.length) { (boff, i) =>
          val j = bInner.indexAt(boff)
          val bValue = bInner.valueAt(boff)
          res.add(i, j, a(i) * bValue)
        }

        res.result(keysAlreadyUnique = true, keysAlreadySorted = true)
      }
    }
  }
}

trait SparseVectorExpandOps extends VectorOps with SparseVector_DenseMatrixOps with SparseVector_GenericOps {

  implicit def liftCSCOpToSVransposeOp[Tag, V, LHS, R](
                                                        implicit op: UFunc.UImpl2[Tag, LHS, CSCMatrix[V], R],
                                                        zero: Zero[V],
                                                        ct: ClassTag[V]): UFunc.UImpl2[Tag, LHS, Transpose[SparseVector[V]], R] =
    new UFunc.UImpl2[Tag, LHS, Transpose[SparseVector[V]], R] {
      def apply(v: LHS, v2: Transpose[SparseVector[V]]): R = {
        op(v, v2.inner.asCscRow)
      }
    }

  @expand
  @expand.valify
  implicit def impl_Op_SV_SV_eq_SV[@expand.args(Int, Double, Float, Long) T, @expand.args(OpAdd, OpSub) Op <: OpType](
                                                                                                                       implicit @expand.sequence[Op]({
    _ + _
  }, {
    _ - _
  }) op: Op.Impl2[T, T, T],
                                                                                                                       @expand.sequence[T](0, 0.0, 0f, 0L) zero: T): Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =
    new Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val asize: Int = a.activeSize
        val bsize: Int = b.activeSize

        val q: T = zero

        val resultI: Array[Int] = new Array[Int](asize + bsize)
        val resultV: Array[T] = new Array[T](asize + bsize)
        var resultOff: Int = 0

        var aoff: Int = 0
        var boff: Int = 0

        // double loop:
        // b moves to catch up with a, then a takes a step (possibly bringing b along)
        while (aoff < asize) {

          while (boff < bsize && b.indexAt(boff) < a.indexAt(aoff)) {
            resultI(resultOff) = b.indexAt(boff)
            resultV(resultOff) = op(q, b.valueAt(boff))
            resultOff += 1
            boff += 1
          }

          val bvalue: T = if (boff < bsize && b.indexAt(boff) == a.indexAt(aoff)) {
            val bv: T = b.valueAt(boff)
            boff += 1
            bv
          } else {
            q
          }
          resultI(resultOff) = a.indexAt(aoff)
          resultV(resultOff) = op(a.valueAt(aoff), bvalue)
          resultOff += 1
          aoff += 1
        }

        while (boff < bsize) {
          resultI(resultOff) = b.indexAt(boff)
          resultV(resultOff) = op(q, b.valueAt(boff))
          resultOff += 1
          boff += 1
        }

        if (resultOff != resultI.length) {
          new SparseVector[T](
            util.Arrays.copyOf(resultI, resultOff),
            util.Arrays.copyOf(resultV, resultOff),
            resultOff,
            a.length)
        } else {
          new SparseVector[T](resultI, resultV, resultOff, a.length)
        }
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  implicit def implSubOp_SV_SV_eq_SV[T: Ring : ClassTag]
  : OpSub.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] = {
    new OpSub.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      val r = implicitly[Ring[T]]

      def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val asize: Int = a.activeSize
        val bsize: Int = b.activeSize

        val q: T = r.zero

        val resultI: Array[Int] = new Array[Int](asize + bsize)
        val resultV: Array[T] = new Array[T](asize + bsize)
        var resultOff: Int = 0

        var aoff: Int = 0
        var boff: Int = 0

        // double loop:
        // b moves to catch up with a, then a takes a step (possibly bringing b along)
        while (aoff < asize) {

          while (boff < bsize && b.indexAt(boff) < a.indexAt(aoff)) {
            resultI(resultOff) = b.indexAt(boff)
            resultV(resultOff) = r.-(q, b.valueAt(boff))
            resultOff += 1
            boff += 1
          }

          val bvalue: T = if (boff < bsize && b.indexAt(boff) == a.indexAt(aoff)) {
            val bv: T = b.valueAt(boff)
            boff += 1
            bv
          } else {
            q
          }
          resultI(resultOff) = a.indexAt(aoff)
          resultV(resultOff) = r.-(a.valueAt(aoff), bvalue)
          resultOff += 1
          aoff += 1
        }

        while (boff < bsize) {
          resultI(resultOff) = b.indexAt(boff)
          resultV(resultOff) = r.-(q, b.valueAt(boff))
          resultOff += 1
          boff += 1
        }

        if (resultOff != resultI.length) {
          val dat = new Array[T](resultOff)
          Array.copy(resultV, 0, dat, 0, resultOff)
          new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), dat, resultOff, a.length)
        } else {
          new SparseVector[T](resultI, resultV, resultOff, a.length)
        }
      }
    }
  }

  implicit def implAddOp_SV_SV_eq_SV[T: Semiring : ClassTag]
  : OpAdd.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] = {
    new OpAdd.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      val r = implicitly[Semiring[T]]

      def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val asize: Int = a.activeSize
        val bsize: Int = b.activeSize

        val q: T = r.zero

        val resultI: Array[Int] = new Array[Int](asize + bsize)
        val resultV: Array[T] = new Array[T](asize + bsize)
        var resultOff: Int = 0

        var aoff: Int = 0
        var boff: Int = 0

        // double loop:
        // b moves to catch up with a, then a takes a step (possibly bringing b along)
        while (aoff < asize) {

          while (boff < bsize && b.indexAt(boff) < a.indexAt(aoff)) {
            resultI(resultOff) = b.indexAt(boff)
            resultV(resultOff) = r.+(q, b.valueAt(boff))
            resultOff += 1
            boff += 1
          }

          val bvalue: T = if (boff < bsize && b.indexAt(boff) == a.indexAt(aoff)) {
            val bv: T = b.valueAt(boff)
            boff += 1
            bv
          } else {
            q
          }
          resultI(resultOff) = a.indexAt(aoff)
          resultV(resultOff) = r.+(a.valueAt(aoff), bvalue)
          resultOff += 1
          aoff += 1
        }

        while (boff < bsize) {
          resultI(resultOff) = b.indexAt(boff)
          resultV(resultOff) = r.+(q, b.valueAt(boff))
          resultOff += 1
          boff += 1
        }

        if (resultOff != resultI.length) {
          val dat = new Array[T](resultOff)
          Array.copy(resultV, 0, dat, 0, resultOff)
          new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), dat, resultOff, a.length)
        } else {
          new SparseVector[T](resultI, resultV, resultOff, a.length)
        }
      }
    }
  }


  @expand
  @expand.valify
  implicit def impl_OpMulScalar_SV_SV_eq_SV[@expand.args(Int, Double, Float, Long) T](
                                                                                      implicit @expand.sequence[T](0, 0.0, 0f, 0L) zero: T)
  : OpMulScalar.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =
    new OpMulScalar.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
        if (b.activeSize < a.activeSize) {
          apply(b, a)
        } else {
          require(b.length == a.length, "Vectors must be the same length!")
          val asize: Int = a.activeSize
          val bsize: Int = b.activeSize

          val resultI: Array[Int] = new Array[Int](math.min(asize, bsize))
          val resultV: Array[T] = new Array[T](math.min(asize, bsize))
          var resultOff: Int = 0

          var aoff: Int = 0
          var boff: Int = 0
          // in principle we could do divide and conquer here
          // by picking the middle of a, figuring out where that is in b, and then recursing,
          // using it as a bracketing.

          // double loop:
          // b moves to catch up with a, then a takes a step (possibly bringing b along)
          while (aoff < asize) {
            val aind: Int = a.indexAt(aoff)
            // the min reflects the invariant that index aind must be in the first aind active indices in b's index.
            boff = util.Arrays.binarySearch(b.index, boff, math.min(bsize, aind + 1), aind)
            if (boff < 0) {
              boff = ~boff
              if (boff == bsize) {
                // we're through the b array, so we're done.
                aoff = asize
              } else {
                // fast forward a until we get to the b we just got to
                val bind = b.indexAt(boff)
                var newAoff = util.Arrays.binarySearch(a.index, aoff, math.min(asize, bind + 1), bind)
                if (newAoff < 0) {
                  newAoff = ~newAoff
                  boff += 1
                }
                assert(
                  newAoff > aoff,
                  s"$bind $aoff $newAoff ${a.index(aoff)} ${a.index(newAoff)} $a $b")
                aoff = newAoff
              }
            } else {
              // b is there, a is there, do the multiplication!
              resultI(resultOff) = aind
              resultV(resultOff) = a.valueAt(aoff) * b.valueAt(boff)
              aoff += 1
              boff += 1
              resultOff += 1
            }
          }

          if (resultOff != resultI.length) {
            new SparseVector[T](
              util.Arrays.copyOf(resultI, resultOff),
              util.Arrays.copyOf(resultV, resultOff),
              resultOff,
              a.length)
          } else {
            new SparseVector[T](resultI, resultV, resultOff, a.length)
          }
        }
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_Op_SV_SV_eq_SV[
    @expand.args(Int, Double, Float, Long) T,
    @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType](implicit @expand.sequence[Op](_ / _, (__x, __y) => __y, _ % _
  , _.pow(_)) op: Op.Impl2[T, T, T]): Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =
    new Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        var i: Int = 0
        while (i < a.length) {
          result.add(i, op(a(i), b(i)))
          i += 1
        }
        result.toSparseVector(true, true)
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_Op_SV_V_eq_SV[
    @expand.args(Int, Double, Float, Long) T,
    @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
                                                            implicit @expand.sequence[Op](
    _ / _
    , (__x, __y) => __y,
    _ % _,
    _.pow(_)
  )
  op: Op.Impl2[T, T, T]): Op.Impl2[SparseVector[T], Vector[T], SparseVector[T]] =
    new Op.Impl2[SparseVector[T], Vector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: Vector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        cforRange(0 until a.length) { i =>
          result.add(i, op(a(i), b(i)))
        }
        result.toSparseVector(true, true)
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }


  @expand
  @expand.valify
  implicit def impl_Op_SV_S_eq_SV[
    @expand.args(Int, Double, Float, Long) T,
    @expand.args(OpAdd, OpSub, OpSet, OpPow) Op <: OpType](
                                                            implicit @expand.sequence[Op]({
    _ + _
  }, {
    _ - _
  }, { (__x, __y) => __y }, {
    _.pow(_)
  }) op: Op.Impl2[T, T, T],
                                                            @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T): Op.Impl2[SparseVector[T], T, SparseVector[T]] =
    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)

        cforRange(0 until a.length) { i =>
          val r = op(a(i), b)
          if (r != zero)
            result.add(i, r)
        }
        result.toSparseVector(true, true)
      }

      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_Op_SV_S_eq_SV[@expand.args(Int, Double, Float, Long) T, @expand.args(OpDiv, OpMod) Op <: OpType](
                                                                                                                      implicit @expand.sequence[Op](
    _ / _
  ,
    _ % _
  ) op: Op.Impl2[T, T, T],
                                                                                                                      @expand.sequence[T](0, 0.0, 0.0f, 0L) zero: T): Op.Impl2[SparseVector[T], T, SparseVector[T]] =
    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        if (b == zero) {
          cforRange(0 until a.length) { i =>
            val r = op(a(i), b)
            if (r != zero)
              result.add(i, r)
          }
        } else {
          cforRange(0 until a.activeSize) { i =>
            val r = op(a.valueAt(i), b)
            if (r != zero)
              result.add(a.indexAt(i), r)
          }
        }
        result.toSparseVector(true, true)
      }

      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_Op_SV_S_eq_SV[
    @expand.args(Int, Double, Float, Long) T,
    @expand.args(OpMulScalar, OpMulMatrix) Op <: OpType](
                                                          implicit @expand.sequence[T](0, 0.0, 0.0f, 0L)
  zero: T): Op.Impl2[SparseVector[T], T, SparseVector[T]] =
    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)

        var i: Int = 0
        while (i < a.activeSize) {
          result.add(a.indexAt(i), a.valueAt(i) * b)
          i += 1
        }
        result.toSparseVector(true, true)
      }

      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_OpMulInner_SV_SV_eq_T[@expand.args(Int, Double, Float, Long) T](
                                                                                    implicit @expand.sequence[T](0, 0.0, 0f, 0L) zero: T): OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] =
    new OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] {
      def apply(a: SparseVector[T], b: SparseVector[T]): T = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (b.activeSize < a.activeSize) {
          apply(b, a)
        } else if (a.activeSize == 0) {
          zero
        } else if (b.activeSize <= 32) { // b is bigger than a
          smallVectors(a, b)
        } else {
          bigVectors(a, b)
        }
      }

      def smallVectors(a: SparseVector[T], b: SparseVector[T]): T = {
        val asize: Int = a.activeSize
        val bsize: Int = b.activeSize

        var result: T = zero
        var aoff: Int = 0
        var boff: Int = 0

        while (aoff < asize && boff < bsize) {
          if (a.indexAt(aoff) < b.indexAt(boff))
            aoff += 1
          else if (b.indexAt(boff) < a.indexAt(aoff))
            boff += 1
          else {
            result += a.valueAt(aoff) * b.valueAt(boff)
            aoff += 1
            boff += 1
          }
        }
        result
      }

      def bigVectors(a: SparseVector[T], b: SparseVector[T]): T = {
        val asize: Int = a.activeSize
        val bsize: Int = b.activeSize

        var result: T = zero

        var aoff: Int = 0
        var boff: Int = 0
        // used for finding upper bounds for location of b
        //        var bLastOff = 0
        //        var bLastInd = 0
        // in principle we could do divide and conquer here
        // by picking the middle of a, figuring out where that is in b, and then recursing,
        // using it as a bracketing.

        // double loop:
        // b moves to catch up with a, then a takes a step (possibly bringing b along)
        while (aoff < asize) {
          val aind: Int = a.indexAt(aoff)
          val bMax = math.min(bsize, aind + 1)
          //math.min(bsize, bLastOff + aind - bLastInd + 1)
          // we use gallop search because we expect aind to be closish to b.index(boff)
          boff = ArrayUtil.gallopSearch(b.index, boff, bMax, aind)
          if (boff < 0) {
            boff = ~boff
            if (boff == bsize) {
              // we're through the b array, so we're done.
              aoff = asize
            } else {
              // fast forward a until we get to the b we just got to
              val bind: Int = b.indexAt(boff)
              //              bLastOff = boff
              //              bLastInd = bind
              //              val aMax = math.min(asize, aoff + bind  - aind + 1)
              val aMax = math.min(asize, bind + 1)
              var newAoff: Int = ArrayUtil.gallopSearch(a.index, aoff, aMax, bind)
              if (newAoff < 0) {
                newAoff = ~newAoff
                boff += 1
              }
              aoff = newAoff
            }
          } else {
            //            bLastOff = boff
            //            bLastInd = aind
            // b is there, a is there, do the multiplication!
            result += a.valueAt(aoff) * b.valueAt(boff)
            aoff += 1
            boff += 1
          }
        }

        result
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }

  implicit def impl_OpMulInner_SV_SV_eq_T[T: ClassTag : Zero : Semiring]
  : OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] =
    new OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] {
      val s = implicitly[Semiring[T]]

      def apply(a: SparseVector[T], b: SparseVector[T]): T = {
        if (b.activeSize < a.activeSize) {
          apply(b, a)
        } else {
          require(b.length == a.length, "Vectors must be the same length!")
          val asize: Int = a.activeSize
          val bsize: Int = b.activeSize

          var result: T = s.zero

          var aoff: Int = 0
          var boff: Int = 0
          // in principle we could do divide and conquer here
          // by picking the middle of a, figuring out where that is in b, and then recursing,
          // using it as a bracketing.

          // double loop:
          // b moves to catch up with a, then a takes a step (possibly bringing b along)
          while (aoff < asize) {
            val aind: Int = a.indexAt(aoff)
            boff = util.Arrays.binarySearch(b.index, boff, math.min(bsize, aind + 1), aind)
            if (boff < 0) {
              boff = ~boff
              if (boff == bsize) {
                // we're through the b array, so we're done.
                aoff = asize
              } else {
                // fast forward a until we get to the b we just got to
                val bind: Int = b.indexAt(boff)
                var newAoff: Int = util.Arrays.binarySearch(a.index, aoff, math.min(asize, bind + 1), bind)
                if (newAoff < 0) {
                  newAoff = ~newAoff
                  boff += 1
                }
                assert(newAoff > aoff, s"$aoff $newAoff")
                aoff = newAoff
              }
            } else {
              // b is there, a is there, do the multiplication!
              result = s.+(result, s.*(a.valueAt(aoff), b.valueAt(boff)))
              aoff += 1
              boff += 1
            }
          }

          result
        }
      }
    }

  @expand
  @expand.valify
  implicit def implScaleAdd_SV_S_SV_InPlace[@expand.args(Int, Double, Float, Long) T](
                                                                                       implicit @expand.sequence[T](0, 0.0, 0f, 0L) zero: T)
  : scaleAdd.InPlaceImpl3[SparseVector[T], T, SparseVector[T]] =
    new scaleAdd.InPlaceImpl3[SparseVector[T], T, SparseVector[T]] {
      def apply(y: SparseVector[T], a: T, x: SparseVector[T]): Unit = {
        require(x.length == y.length, "Vectors must be the same length!")
        val asize: Int = y.activeSize
        val bsize: Int = x.activeSize

        if (a != zero) {

          val resultI: Array[Int] = new Array[Int](asize + bsize)
          val resultV: Array[T] = new Array[T](asize + bsize)
          var resultOff: Int = 0

          var aoff: Int = 0
          var boff: Int = 0

          // double loop:
          // b moves to catch up with a, then a takes a step (possibly bringing b along)
          while (aoff < asize) {

            while (boff < bsize && x.indexAt(boff) < y.indexAt(aoff)) {
              resultI(resultOff) = x.indexAt(boff)
              resultV(resultOff) = a * x.valueAt(boff)
              resultOff += 1
              boff += 1
            }

            val bvalue: T = if (boff < bsize && x.indexAt(boff) == y.indexAt(aoff)) {
              val bv: T = a * x.valueAt(boff)
              boff += 1
              bv
            } else {
              zero
            }
            resultI(resultOff) = y.indexAt(aoff)
            resultV(resultOff) = y.valueAt(aoff) + bvalue
            resultOff += 1
            aoff += 1
          }

          while (boff < bsize) {
            resultI(resultOff) = x.indexAt(boff)
            resultV(resultOff) = a * x.valueAt(boff)
            resultOff += 1
            boff += 1
          }

          if (resultOff != resultI.length) {
            y.use(util.Arrays.copyOf(resultI, resultOff), util.Arrays.copyOf(resultV, resultOff), resultOff)
          } else {
            y.use(resultI, resultV, resultOff)
          }
        }
      }

      implicitly[TernaryUpdateRegistry[Vector[T], T, Vector[T], scaleAdd.type]].register(this)
    }

  class CanZipMapValuesSparseVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV: ClassTag : Zero : Semiring]
    extends CanZipMapValues[SparseVector[V], V, RV, SparseVector[RV]] {
    def create(length: Int): SparseVector[RV] = SparseVector.zeros(length)

    def map(from: SparseVector[V], from2: SparseVector[V], fn: (V, V) => RV): SparseVector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")

      val zz = fn(from.default, from2.default)
      if (zz != implicitly[Zero[RV]].zero) {

        val result: SparseVector[RV] = create(from.length)
        var i = 0
        while (i < from.length) {
          result(i) = fn(from(i), from2(i))
          i += 1
        }
        result
      } else {
        val vb = new VectorBuilder[RV](from.length)
        var off1, off2 = 0
        while (off1 < from.activeSize) {
          while (off2 < from2.activeSize && from2.indexAt(off2) < from.indexAt(off1)) {
            val index = from2.indexAt(off2)
            vb.add(index, fn(from.default, from2.valueAt(off2)))
            off2 += 1
          }

          if (off2 < from2.activeSize && from.indexAt(off1) == from2.indexAt(off2)) {
            val index = from2.indexAt(off2)
            vb.add(index, fn(from.valueAt(off1), from2.valueAt(off2)))
            off2 += 1
          } else {
            val index = from.indexAt(off1)
            vb.add(index, fn(from.valueAt(off1), from2.default))
          }

          off1 += 1
        }

        while (off2 < from2.activeSize) {
          val index = from2.indexAt(off2)
          vb.add(index, fn(from.default, from2.valueAt(off2)))
          off2 += 1
        }

        vb.toSparseVector(true, true)
      }
    }
  }

  implicit def zipMap[V, R: ClassTag : Zero : Semiring]: CanZipMapValuesSparseVector[V, R] =
    new CanZipMapValuesSparseVector[V, R]

  implicit val zipMap_d: CanZipMapValuesSparseVector[Double, Double] = new CanZipMapValuesSparseVector[Double, Double]
  implicit val zipMap_f: CanZipMapValuesSparseVector[Float, Float] = new CanZipMapValuesSparseVector[Float, Float]
  implicit val zipMap_i: CanZipMapValuesSparseVector[Int, Int] = new CanZipMapValuesSparseVector[Int, Int]

  class CanZipMapKeyValuesSparseVector[
    @spec(Double, Int, Float, Long) V, @spec(Int, Double) RV: ClassTag : Zero : Semiring]
    extends CanZipMapKeyValues[SparseVector[V], Int, V, RV, SparseVector[RV]] {
    def create(length: Int): SparseVector[RV] = SparseVector.zeros(length)

    def map(from: SparseVector[V], from2: SparseVector[V], fn: (Int, V, V) => RV): SparseVector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result: SparseVector[RV] = create(from.length)
      cforRange (0 until from.length) { i =>
        result(i) = fn(i, from(i), from2(i))
      }
      result
    }

    override def mapActive(from: SparseVector[V], from2: SparseVector[V], fn: (Int, V, V) => RV): SparseVector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")
      val vb = new VectorBuilder[RV](from.length)
      var off1, off2 = 0
      while (off1 < from.activeSize) {
        while (off2 < from2.activeSize && from2.indexAt(off2) < from.indexAt(off1)) {
          val index = from2.indexAt(off2)
          vb.add(index, fn(index, from.default, from2.valueAt(off2)))
          off2 += 1
        }

        if (off2 < from2.activeSize && from.indexAt(off1) == from2.indexAt(off2)) {
          val index = from2.indexAt(off2)
          vb.add(index, fn(index, from.valueAt(off1), from2.valueAt(off2)))
          off2 += 1
        } else {
          val index = from.indexAt(off1)
          vb.add(index, fn(index, from.valueAt(off1), from2.default))
        }

        off1 += 1
      }

      while (off2 < from2.activeSize) {
        val index = from2.indexAt(off2)
        vb.add(index, fn(index, from.default, from2.valueAt(off2)))
        off2 += 1
      }

      vb.toSparseVector(true, true)
    }
  }

  implicit def zipMapKV[V, R: ClassTag : Zero : Semiring]: CanZipMapKeyValuesSparseVector[V, R] =
    new CanZipMapKeyValuesSparseVector[V, R]
}


trait SparseVector_DenseMatrixOps extends DenseMatrixMultiplyOps {
  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_DM_SV_eq_DV[@expand.args(Int, Float, Long, Double) T]
    : OpMulMatrix.Impl2[DenseMatrix[T], SparseVector[T], DenseVector[T]] = {
    new OpMulMatrix.Impl2[DenseMatrix[T], SparseVector[T], DenseVector[T]] {
      override def apply(v: DenseMatrix[T], v2: SparseVector[T]): DenseVector[T] = {
        require(v.cols == v2.length)
        val result = DenseVector.zeros[T](v.rows)
        cforRange(0 until v2.activeSize) { i =>
          axpy(v2.valueAt(i), v(::, v2.indexAt(i)), result)
        }

        result
      }
      implicitly[BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix.type, DenseVector[T]]].register(this)
    }

  }

}