package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.support.{CanZipMapKeyValues, CanZipMapValues}
import breeze.macros.{cforRange, expand}
import breeze.math.{Field, Ring, Semiring}
import breeze.storage.Zero
import breeze.util.{ArrayUtil, ReflectionUtil}
import scalaxy.debug.{assert, require}

import java.util
import scala.reflect.ClassTag

// TODO: names
trait Vector_GenericOps extends GenericOps with Vector_TraversalOps {
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
                                  ct: ClassTag[T]): OpAdd.InPlaceImpl2[Vector[T], T] = {
    new OpAdd.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field.+(v(i), v2)
      }
    }
  }

  implicit def vSubIntoSField[T](
                                  implicit field: Ring[T],
                                  ct: ClassTag[T]): OpSub.InPlaceImpl2[Vector[T], T] = {
    new OpSub.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field.-(v(i), v2)
      }
    }

  }

  implicit def vMulScalarIntoSField[T](
                                        implicit field: Semiring[T],
                                        ct: ClassTag[T]): OpMulScalar.InPlaceImpl2[Vector[T], T] = {
    new OpMulScalar.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field.*(v(i), v2)
      }
    }
  }

  implicit def vDivIntoSField[T](
                                  implicit field: Field[T],
                                  ct: ClassTag[T]): OpDiv.InPlaceImpl2[Vector[T], T] = {
    new OpDiv.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for (i <- 0 until v.length) v(i) = field./(v(i), v2)
      }
    }
  }

  implicit def vPowIntoS[T](
                             implicit pow: OpPow.Impl2[T, T, T],
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

        cforRange(0 until a.length) { i =>
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

trait SparseVector_GenericOps extends GenericOps {
  implicit def impl_OpSet_InPlace_SV_SV_Generic[T]: OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]] = {
    new OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]): Unit = {
        val result = b.copy
        a.use(result.index, result.data, result.activeSize)
      }
    }
  }

  implicit def impl_OpSet_InPlace_SV_S_Generic[T: Zero]: OpSet.InPlaceImpl2[SparseVector[T], T] = {
    val zero = implicitly[Zero[T]].zero
    new OpSet.InPlaceImpl2[SparseVector[T], T] {
      def apply(a: SparseVector[T], b: T): Unit = {
        implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)
        if (b == zero) {
          a.use(new Array[Int](2), new Array[T](2), 0)
        } else {
          val data = Array.fill(a.length)(b)
          val index = Array.range(0, a.length)
          a.use(index, data, a.length)
        }
      }
    }
  }

  implicit def impl_Op_SV_SV_InPlace_liftFromPure[Op, T, U, V](
      implicit
      pureOp: UFunc.UImpl2[Op, SparseVector[T], U, V],
      opSet: OpSet.InPlaceImpl2[SparseVector[T], V]): UFunc.InPlaceImpl2[Op, SparseVector[T], U] = {
    (sv: SparseVector[T], u: U) =>
    val r = pureOp(sv, u)
    opSet(sv, r)
  }


  // TODO: this may not be necessary?
  implicit def implOp_SV_S_eq_SV_Generic[Op <: OpType, T: Semiring](
      implicit op: UFunc.UImpl2[Op, T, T, T]): UFunc.UImpl2[Op, SparseVector[T], T, SparseVector[T]] = {
    (a: SparseVector[T], b: T) => {
      implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)
      val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
      val f = implicitly[Semiring[T]]
      cforRange (0 until a.length) { i =>
        val r = op(a(i), b)
        if (r != f.zero)
          result.add(i, r)
      }
      result.toSparseVector(true, true)
    }
  }

  implicit def impl_OpMulScalar_SV_S_eq_SV_Generic[T: Semiring: ClassTag]
    : OpMulScalar.Impl2[SparseVector[T], T, SparseVector[T]] =
    (a: SparseVector[T], b: T) => {
      val f = implicitly[Semiring[T]]
      val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
      if (b != f.zero) {
        cforRange (0 until a.activeSize) { i =>
          result.add(a.indexAt(i), f.*(a.valueAt(i), b))
        }
      }
      result.toSparseVector(true, true)
    }

  implicit def impl_Op_SV_S_eq_SV_Generic_OpMulMatrix[T: Semiring: ClassTag]
    : OpMulMatrix.Impl2[SparseVector[T], T, SparseVector[T]] =
    (a: SparseVector[T], b: T) => {
      val f = implicitly[Semiring[T]]
      val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
      if (b != f.zero) {
        cforRange (0 until a.activeSize) { i =>
          result.add(a.indexAt(i), f.*(a.valueAt(i), b))
        }
      }
      result.toSparseVector(true, true)
    }

  implicit def impl_Op_SV_SV_eq_SV_Generic_OpMulScalar[T](implicit ring: Semiring[T])
    : OpMulScalar.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =
    new OpMulScalar.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]) = {
        implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)
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
                assert(newAoff > aoff, s"$bind $aoff $newAoff ${a.index(aoff)} ${a.index(newAoff)} $a $b")
                aoff = newAoff
              }
            } else {
              // b is there, a is there, do the multiplication!
              resultI(resultOff) = aind
              resultV(resultOff) = ring.*(a.valueAt(aoff), b.valueAt(boff))
              aoff += 1
              boff += 1
              resultOff += 1
            }
          }

          if (resultOff != resultI.length) {
            new SparseVector[T](
              util.Arrays.copyOf(resultI, resultOff),
              breeze.util.ArrayUtil.copyOf(resultV, resultOff),
              resultOff,
              a.length)
          } else {
            new SparseVector[T](resultI, resultV, resultOff, a.length)
          }
        }
      }
    }

  @expand
  implicit def impl_scaleAdd_SV_S_SV_InPlace_Generic[T: Semiring: ClassTag]
    : scaleAdd.InPlaceImpl3[SparseVector[T], T, SparseVector[T]] =
    (dest: SparseVector[T], scale: T, source: SparseVector[T]) => {
      val f = implicitly[Semiring[T]]
      require(source.length == dest.length, "Vectors must be the same length!")
      val asize: Int = dest.activeSize
      val bsize: Int = source.activeSize

      if (scale != f.zero && bsize != 0) {
        val resultI: Array[Int] = new Array[Int](asize + bsize)
        val resultV: Array[T] = new Array[T](asize + bsize)
        var resultOff: Int = 0

        var aoff: Int = 0
        var boff: Int = 0

        // double loop:
        // b moves to catch up with a, then a takes a step (possibly bringing b along)
        while (aoff < asize) {

          while (boff < bsize && source.indexAt(boff) < dest.indexAt(aoff)) {
            resultI(resultOff) = source.indexAt(boff)
            resultV(resultOff) = f.*(scale, source.valueAt(boff))
            resultOff += 1
            boff += 1
          }

          val bvalue: T = if (boff < bsize && source.indexAt(boff) == dest.indexAt(aoff)) {
            val bv: T = f.*(scale, source.valueAt(boff))
            boff += 1
            bv
          } else {
            f.zero
          }
          resultI(resultOff) = dest.indexAt(aoff)
          resultV(resultOff) = f.+(dest.valueAt(aoff), bvalue)
          resultOff += 1
          aoff += 1
        }

        while (boff < bsize) {
          resultI(resultOff) = source.indexAt(boff)
          resultV(resultOff) = f.*(scale, source.valueAt(boff))
          resultOff += 1
          boff += 1
        }

        if (resultOff != resultI.length) {
          dest.use(util.Arrays.copyOf(resultI, resultOff), ArrayUtil.copyOf[T](resultV, resultOff), resultOff)
        } else {
          dest.use(resultI, resultV, resultOff)
        }
      }
    }

  implicit def impl_Op_SV_SV_eq_SV_Generic[T, Op <: OpType](
      implicit
      op: UFunc.UImpl2[Op, T, T, T], semiring: Semiring[T]): UFunc.UImpl2[Op, SparseVector[T], SparseVector[T], SparseVector[T]] =
    (a: SparseVector[T], b: SparseVector[T]) => {
      require(b.length == a.length, "Vectors must be the same length!")
      implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)
      val zero = implicitly[Semiring[T]].zero
      val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
      var i: Int = 0
      while (i < a.length) {
        val r = op(a(i), b(i))
        if (r != zero)
          result.add(i, r)
        i += 1
      }
      result.toSparseVector(true, true)
    }

}

trait DenseVector_GenericOps extends VectorOps {

  implicit def impl_OpSet_InPlace_DV_V_Generic[V]: OpSet.InPlaceImpl2[DenseVector[V], V] =
    new OpSet.InPlaceImpl2[DenseVector[V], V] {
      def apply(a: DenseVector[V], b: V): Unit = {
        val ad: Array[V] = a.data
        if (a.stride == 1) {
          ArrayUtil.fill(ad, a.offset, a.length, b)
        } else {
          var aoff = a.offset
          cforRange (0 until a.length) { i =>
            ad(aoff) = b
            aoff += a.stride
          }
        }
      }
    }

  implicit def impl_OpSet_InPlace_DV_DV[V]: OpSet.InPlaceImpl2[DenseVector[V], DenseVector[V]] =
    new OpSet.InPlaceImpl2[DenseVector[V], DenseVector[V]] {
      def apply(a: DenseVector[V], b: DenseVector[V]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (a.overlaps(b)) {
          apply(a, b.copy)
        } else if (a.stride == b.stride && a.stride == 1) {
          System.arraycopy(b.data, b.offset, a.data, a.offset, a.length)
        } else {
          val ad: Array[V] = a.data
          val bd: Array[V] = b.data
          var aoff = a.offset
          var boff = b.offset

          cforRange (0 until a.length) { i =>
            ad(aoff) = bd(boff)
            aoff += a.stride
            boff += b.stride
          }
        }

      }
    }

  implicit def impl_scaleAdd_InPlace_DV_S_DV_Generic[T: Semiring]: scaleAdd.InPlaceImpl3[DenseVector[T], T, DenseVector[T]] =
    new scaleAdd.InPlaceImpl3[DenseVector[T], T, DenseVector[T]] {
      val ring = implicitly[Semiring[T]]
      def apply(a: DenseVector[T], s: T, b: DenseVector[T]): Unit = {
        if (a.overlaps(b)) {
          apply(a, s, b.copy)
        } else {
          require(b.length == a.length, "Vectors must be the same length!")
          val ad: Array[T] = a.data
          val bd: Array[T] = b.data
          var aoff = a.offset
          var boff = b.offset

          var i = 0
          while (i < a.length) {
            ad(aoff) = ring.+(ad(aoff), ring.*(s, bd(boff)))
            aoff += a.stride
            boff += b.stride
            i += 1
          }
        }
      }
    }

  implicit def impl_OpSet_InPlace_DV_V[T, Vec](
                                                implicit ev: Vec <:< Vector[T]): OpSet.InPlaceImpl2[DenseVector[T], Vec] =
    new OpSet.InPlaceImpl2[DenseVector[T], Vec] {
      def apply(a: DenseVector[T], b: Vec): Unit = {
        val ad: Array[T] = a.data
        var aoff: Int = a.offset

        var i = 0
        while (i < a.length) {
          ad(aoff) = b(i)
          aoff += a.stride
          i += 1
        }
      }
      //    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], OpSet.type]].register(this)
    }

  implicit def impl_Op_LHS_DVt_eq_R_cast[Tag, V, LHS, R](
                                                          implicit op: UFunc.UImpl2[Tag, LHS, DenseMatrix[V], R]): UFunc.UImpl2[Tag, LHS, Transpose[DenseVector[V]], R] =
    new UFunc.UImpl2[Tag, LHS, Transpose[DenseVector[V]], R] {
      def apply(v: LHS, v2: Transpose[DenseVector[V]]): R = {
        val dv: DenseVector[V] = v2.inner
        val dm: DenseMatrix[V] =
          DenseMatrix.create(data = dv.data, offset = dv.offset, cols = dv.length, rows = 1, majorStride = dv.stride)
        op(v, dm)
      }
    }

  //  TODO: try removing
  implicit def impl_OpAdd_InPlace_DV_DV_Generic[T](
                                                    implicit field: Semiring[T],
                                                    ): OpAdd.InPlaceImpl2[DenseVector[T], DenseVector[T]] = {
    new OpAdd.InPlaceImpl2[DenseVector[T], DenseVector[T]] {
      override def apply(v: DenseVector[T], v2: DenseVector[T]) = {
        if (v.overlaps(v2)) {
          apply(v, v2.copy)
        } else {
          cforRange (0 until v.length) { i =>
            v(i) = field.+(v(i), v2(i))
          }
        }
      }
    }
  }

  implicit def impl_OpSub_InPlace_DV_DV_Generic[T](implicit field: Ring[T]): OpSub.InPlaceImpl2[DenseVector[T], DenseVector[T]] = {
    new OpSub.InPlaceImpl2[DenseVector[T], DenseVector[T]] {
      override def apply(v: DenseVector[T], v2: DenseVector[T]) = {
        if (v.overlaps(v2)) {
          apply(v, v2.copy)
        } else {
          cforRange (0 until v.length) { i =>
            v(i) = field.-(v(i), v2(i))
          }
        }
      }
    }

  }

  implicit def impl_OpMulScalar_InPlace_DV_DV_Generic[T](implicit field: Semiring[T]): OpMulScalar.InPlaceImpl2[DenseVector[T], DenseVector[T]] = {
    new OpMulScalar.InPlaceImpl2[DenseVector[T], DenseVector[T]] {
      override def apply(v: DenseVector[T], v2: DenseVector[T]) = {
        if (v.overlaps(v2)) {
          apply(v, v2.copy)
        } else {
          cforRange (0 until v.length) { i =>
            v(i) = field.*(v(i), v2(i))
          }
        }
      }
    }

  }

  implicit def impl_OpDiv_InPlace_DV_DV_Generic[T](implicit field: Field[T]): OpDiv.InPlaceImpl2[DenseVector[T], DenseVector[T]] = {
    new OpDiv.InPlaceImpl2[DenseVector[T], DenseVector[T]] {
      override def apply(v: DenseVector[T], v2: DenseVector[T]) = {
        if (v.overlaps(v2)) {
          apply(v, v2.copy)
        } else {
          cforRange (0 until v.length) { i =>
            v(i) = field./(v(i), v2(i))
          }
        }
      }
    }

  }

  implicit def impl_OpPow_InPlace_DV_DV_Generic[T](implicit pow: OpPow.Impl2[T, T, T]): OpPow.InPlaceImpl2[DenseVector[T], DenseVector[T]] = {
    new OpPow.InPlaceImpl2[DenseVector[T], DenseVector[T]] {
      override def apply(v: DenseVector[T], v2: DenseVector[T]) = {
        if (v.overlaps(v2)) {
          apply(v, v2.copy)
        } else {
          cforRange (0 until v.length) { i =>
            v(i) = pow(v(i), v2(i))
          }
        }
      }
    }

  }

  implicit def impl_OpAdd_InPlace_DV_S_Generic[T](implicit field: Semiring[T]): OpAdd.InPlaceImpl2[DenseVector[T], T] = {
    new OpAdd.InPlaceImpl2[DenseVector[T], T] {
      override def apply(v: DenseVector[T], v2: T) = {
        cforRange (0 until v.length) { i =>
          v(i) = field.+(v(i), v2)
        }
      }
    }

  }

  implicit def impl_OpSub_InPlace_DV_S_Generic[T](implicit field: Ring[T]): OpSub.InPlaceImpl2[DenseVector[T], T] = {
    new OpSub.InPlaceImpl2[DenseVector[T], T] {
      override def apply(v: DenseVector[T], v2: T) = {
        cforRange (0 until v.length) { i =>
          v(i) = field.-(v(i), v2)
        }
      }
    }

  }

  implicit def impl_OpMulScalar_InPlace_DV_S_Generic[T](implicit field: Semiring[T]): OpMulScalar.InPlaceImpl2[DenseVector[T], T] = {
    new OpMulScalar.InPlaceImpl2[DenseVector[T], T] {
      override def apply(v: DenseVector[T], v2: T) = {
        cforRange (0 until v.length) { i =>
          v(i) = field.*(v(i), v2)
        }
      }
    }
  }

  implicit def impl_OpDiv_InPlace_DV_S_Generic[T](implicit field: Field[T]): OpDiv.InPlaceImpl2[DenseVector[T], T] = {
    new OpDiv.InPlaceImpl2[DenseVector[T], T] {
      override def apply(v: DenseVector[T], v2: T) = {
        cforRange (0 until v.length) { i =>
          v(i) = field./(v(i), v2)
        }
      }
    }
  }

  implicit def impl_OpPow_InPlace_DV_S_Generic[T](implicit pow: OpPow.Impl2[T, T, T]): OpPow.InPlaceImpl2[DenseVector[T], T] = {
    new OpPow.InPlaceImpl2[DenseVector[T], T] {
      override def apply(v: DenseVector[T], v2: T) = {
        cforRange (0 until v.length) { i =>
          v(i) = pow(v(i), v2)
        }
      }
    }
  }

  // todo: try removing
  implicit def impl_OpMulInner_DV_DV_eq_S_Generic[T](implicit field: Semiring[T]): OpMulInner.Impl2[DenseVector[T], DenseVector[T], T] = {
    new OpMulInner.Impl2[DenseVector[T], DenseVector[T], T] {
      override def apply(v: DenseVector[T], v2: DenseVector[T]): T = {
        var acc = field.zero
        cforRange(0 until v.length) { i =>
          acc = field.+(acc, field.*(v(i), v2(i)))
        }
        acc
      }
    }
  }
}

trait HashVector_GenericOps {

  implicit def impl_OpSet_InPlace_HV_S_Generic[V]: OpSet.InPlaceImpl2[HashVector[V], V] = {
    new OpSet.InPlaceImpl2[HashVector[V], V] {
      def apply(a: HashVector[V], b: V): Unit = {
        if (b == a.default) {
          a.clear()
        } else {
          cforRange(0 until a.length) { i =>
            a(i) = b
          }
        }
      }
    }
  }

  implicit def impl_OpSet_InPlace_HV_V_Generic[V]: OpSet.InPlaceImpl2[HashVector[V], Vector[V]] = {
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

  implicit def impl_OpSet_InPlace_HV_HV_Generic[V]: OpSet.InPlaceImpl2[HashVector[V], HashVector[V]] = {
    new OpSet.InPlaceImpl2[HashVector[V], HashVector[V]] {
      def apply(a: HashVector[V], b: HashVector[V]): Unit = {
        require(b.length == a.length, "HashVectors must be the same length!")
        a.clear()
        b.array.copyTo(a.array)
      }
    }
  }

  implicit def impl_scaleAdd_InPlace_HV_V_HV[V: Semiring]: scaleAdd.InPlaceImpl3[HashVector[V], V, HashVector[V]] = {
    new scaleAdd.InPlaceImpl3[HashVector[V], V, HashVector[V]] {
      val ring: Semiring[V] = implicitly[Semiring[V]]
      def apply(a: HashVector[V], s: V, b: HashVector[V]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (s == ring.zero)
          return

        for ((k, v) <- b.activeIterator)
          a(k) = ring.+(a(k), ring.*(s, v))
      }
    }
  }

  class CanZipMapValuesHashVector[@specialized(Double, Int, Float, Long) V, @specialized(Int, Double) RV: ClassTag: Zero]
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

  class CanZipMapKeyValuesHashVector[@specialized(Double, Int, Float, Long) V, @specialized(Int, Double) RV: ClassTag: Zero]
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

  implicit def impl_OpMulScalar_InPlace_HV_HV_Generic[T](
                                                          implicit field: Field[T],
                                                          ct: ClassTag[T]): OpMulScalar.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpMulScalar.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for (i <- 0 until v.length) v(i) = field.*(v(i), v2(i))
      }
    }

  }

  implicit def impl_OpDiv_InPlace_HV_HV_Generic[T](implicit field: Field[T]): OpDiv.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpDiv.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for (i <- 0 until v.length) v(i) = field./(v(i), v2(i))
      }
    }

  }

  implicit def impl_OpMulScalar_InPlace_HV_S_Generic[T](implicit field: Semiring[T]): OpMulScalar.InPlaceImpl2[HashVector[T], T] = {
    new OpMulScalar.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T): Unit = {
        if (v2 == field.zero) {
          v.clear()
        } else if (v2 == field.one) {
          return
        } else {
          cforRange(0 until v.length) { i =>
            v(i) = field.*(v(i), v2)
          }
        }
      }
    }
  }

  implicit def impl_OpDiv_InPlace_HV_S_Generic[T](implicit field: Field[T]): OpDiv.InPlaceImpl2[HashVector[T], T] = {
    new OpDiv.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        cforRange(0 until v.length) { i =>
          v(i) = field./(v(i), v2)
        }
      }
    }
  }

  // TODO try removing
  implicit def impl_OpPow_InPlace_HV_S_Generic[T](implicit pow: OpPow.Impl2[T, T, T]): OpPow.InPlaceImpl2[HashVector[T], T] = {
    new OpPow.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        cforRange(0 until v.length) { i =>
          v(i) = pow(v(i), v2)
        }
      }
    }
  }

}