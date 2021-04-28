package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.linalg._
import breeze.macros.expand
import breeze.math.{Field, Semiring}
import breeze.storage.Zero
import breeze.util.{ArrayUtil, ReflectionUtil}
import scalaxy.debug.{assert, require}

import java.util
import scala.reflect.ClassTag

trait SparseVector_GenericOps extends GenericOps {
  implicit def impl_OpSet_InPlace_SV_SV_Generic[T]: OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]] = {
    new OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]): Unit = {
        val result = b.copy
        a.use(result.index, result.data, result.activeSize)
      }
    }
  }

  // TODO(2): remove need for CT
  implicit def impl_OpSet_InPlace_SV_T_Generic[T: Zero : ClassTag]: OpSet.InPlaceImpl2[SparseVector[T], T] = {
    val zero = implicitly[Zero[T]].zero
    new OpSet.InPlaceImpl2[SparseVector[T], T] {
      def apply(a: SparseVector[T], b: T): Unit = {
        if (b == zero) {
          a.use(new Array[Int](2), new Array[T](2), 0)
          return
        }
        val data = Array.fill(a.length)(b)
        val index = Array.range(0, a.length)
        a.use(index, data, a.length)
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


  implicit def implOp_SV_T_eq_SV_Generic[Op <: OpType, T: Semiring](
      implicit op: UFunc.UImpl2[Op, T, T, T]): UFunc.UImpl2[Op, SparseVector[T], T, SparseVector[T]] = {
    (a: SparseVector[T], b: T) =>
      {
        implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        val f = implicitly[Semiring[T]]
        var i: Int = 0
        while (i < a.length) {
          val r = op(a(i), b)
          if (r != f.zero)
            result.add(i, r)
          i += 1
        }
        result.toSparseVector(true, true)
      }
  }

  implicit def impl_OpMulScalar_SV_T_eq_SV_Generic[T: Semiring: ClassTag]
    : OpMulScalar.Impl2[SparseVector[T], T, SparseVector[T]] =
    (a: SparseVector[T], b: T) => {
      val f = implicitly[Semiring[T]]
      val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
      if (b != f.zero) {
        var i: Int = 0
        while (i < a.activeSize) {
          result.add(a.indexAt(i), f.*(a.valueAt(i), b))
          i += 1
        }
      }
      result.toSparseVector(true, true)
    }

  implicit def implOps_SV_T_eq_SV_Generic_OpMulMatrix[T: Semiring: ClassTag]
    : OpMulMatrix.Impl2[SparseVector[T], T, SparseVector[T]] =
    (a: SparseVector[T], b: T) => {
      val f = implicitly[Semiring[T]]
      val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
      if (b != f.zero) {
        var i: Int = 0
        while (i < a.activeSize) {
          result.add(a.indexAt(i), f.*(a.valueAt(i), b))
          i += 1
        }
      }
      result.toSparseVector(true, true)
    }

  implicit def implOps_SV_SV_eq_SV_Generic_OpMulScalar[T: Semiring]
    : OpMulScalar.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =
    new OpMulScalar.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]) = {
        implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)
        val ring: Semiring[T] = implicitly[Semiring[T]]
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
  implicit def impl_scaleAdd_SV_T_SV_InPlace_Generic[T: Semiring: ClassTag]
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

  implicit def implOps_SV_SV_eq_SV_Generic[T, Op <: OpType](
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

//  // TODO: try removing
//  implicit def implNorm_SV_D_eq_D_Field[T](implicit f: Field[T]): norm.Impl2[SparseVector[T], Double, Double] =
//    (v: SparseVector[T], n: Double) => {
//      import v._
//      if (n == 1) {
//        var sum: Double = 0.0
//        activeValuesIterator.foreach(v => sum += f.sNorm(v))
//        sum
//      } else if (n == 2) {
//        var sum: Double = 0.0
//        activeValuesIterator.foreach(v => {
//          val nn = f.sNorm(v);
//          sum += nn * nn
//        })
//        math.sqrt(sum)
//      } else if (n == Double.PositiveInfinity) {
//        var max: Double = 0.0
//        activeValuesIterator.foreach(v => {
//          val nn = f.sNorm(v);
//          if (nn > max) max = nn
//        })
//        max
//      } else {
//        var sum: Double = 0.0
//        activeValuesIterator.foreach(v => {
//          val nn = f.sNorm(v);
//          sum += math.pow(nn, n)
//        })
//        math.pow(sum, 1.0 / n)
//      }
//    }
}
