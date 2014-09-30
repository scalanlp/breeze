package breeze.linalg
package operators

import breeze.util.ArrayUtil
import spire.syntax.cfor._
import support._
import scala.reflect.ClassTag
import java.util
import breeze.macros.expand
import breeze.macros.expand.sequence
import breeze.math._
import scala.math.BigInt
import breeze.generic.{UFunc}
import scala.specialized
import breeze.storage.Zero
import breeze.generic.UFunc.{UImpl2, UImpl}
import scala.{specialized=>spec}

trait SparseVector_DenseVector_Ops { this: SparseVector.type =>
  import breeze.math.PowImplicits._

  @expand
  @expand.valify
  implicit def implOps_SVT_DVT_InPlace[@expand.args(Int, Double, Float, Long) T,
                                       @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.InPlaceImpl2[SparseVector[T], DenseVector[T]] =

    new Op.InPlaceImpl2[SparseVector[T], DenseVector[T]] {

      def apply(a: SparseVector[T], b: DenseVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length, a.length)
        val bd: Array[T] = b.data
        val adefault: T  = a.array.default
        var boff:    Int = b.offset
        val asize:   Int = a.activeSize
        val bstride: Int = b.stride
        val ad: Array[T]   = a.data
        val ai: Array[Int] = a.index

        var i = 0
        var j = 0
        while(i < asize) {
          // do defaults until we get to the next aoffset
          val nextBoff: Int = b.offset + ai(i) * bstride
          while(boff < nextBoff) {
            result.add(j, op(adefault, bd(boff)))
            boff += bstride
            j += 1
          }

          result.add(j, op(ad(i), bd(boff)))
          boff += b.stride
          i += 1
          j += 1
        }

        while(boff < bd.length) {
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
  implicit def implOps_SVT_DVT_eq_DVT[@expand.args(Int, Double, Float, Long) T,
                                      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.Impl2[SparseVector[T], DenseVector[T], DenseVector[T]] =

    new Op.Impl2[SparseVector[T], DenseVector[T], DenseVector[T]] {

      def apply(a: SparseVector[T], b: DenseVector[T]): DenseVector[T] = {

        require(a.length == b.length, "Vectors must have the same length")
        val result: DenseVector[T] = DenseVector.zeros[T](a.length)
        val bd: Array[T] = b.data
        val adefault: T = a.array.default
        var boff:    Int = b.offset
        val asize:   Int = a.activeSize
        val bstride: Int = b.stride
        val ad: Array[T] = a.data
        val ai: Array[Int] = a.index

        var i = 0
        var j = 0
        while(i < asize) {
          // do defaults until we get to the next aoffset
          val nextBoff: Int = b.offset + ai(i) * bstride
          while(boff < nextBoff) {
            result(j) = op(adefault, bd(boff))
            boff += bstride
            j += 1
          }

          result(j) = op(ad(i), bd(boff))
          boff += b.stride
          i += 1
          j += 1
        }

        while(boff < bd.length) {
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
  implicit def implOpMulInner_SVT_DVT_eq_T[@expand.args(Int, Double, Float, Long) T]:
  breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], DenseVector[T], T] =

    new breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], DenseVector[T], T] {
      def apply(a: SparseVector[T], b: DenseVector[T]): T = {
        require(b.length == a.length, "Vectors must be the same length!")
        b dot a
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
    }


}



trait DenseVector_SparseVector_Ops { this: SparseVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.valify
  implicit def implOps_DVT_SVT_InPlace[@expand.args(Int, Double, Float, Long) T,
                                       @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.InPlaceImpl2[DenseVector[T], SparseVector[T]] =

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
        while(i < bsize) {
          // do defaults until we get to the next aoffset
          val nextAoff: Int = a.offset + bi(i) * astride
          while(aoff < nextAoff) {
            ad(aoff) = op(ad(aoff), bdefault)
            aoff += astride
          }

          ad(aoff) = op(ad(aoff), bd(i))
          aoff += a.stride
          i += 1
        }

        while(aoff < ad.length) {
          ad(aoff) = op(ad(aoff), bdefault)
          aoff += astride
        }
      }
      implicitly[BinaryUpdateRegistry[DenseVector[T], Vector[T], Op.type]].register(this)
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }

  @expand
  @expand.valify
  implicit def implOps_DVT_SVT_InPlace[@expand.args(Int, Double, Float, Long) T,
                                       @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}) op: Op.Impl2[T, T, T]):
  Op.InPlaceImpl2[DenseVector[T], SparseVector[T]] =

    new Op.InPlaceImpl2[DenseVector[T], SparseVector[T]] {
      def apply(a: DenseVector[T], b: SparseVector[T]): Unit = {
        require(a.length == b.length, "Vectors must have the same length")
        val ad: Array[T] = a.data
        val bd: Array[T] = b.data
        val bi: Array[Int] = b.index
        val bsize: Int = b.iterableSize

        var i: Int = 0
        while(i < bsize) {
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
  implicit def implOpMulInner_DVT_SVT_eq_T[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):
  OpMulInner.Impl2[DenseVector[T], SparseVector[T], T] =

    new OpMulInner.Impl2[DenseVector[T], SparseVector[T], T] {
      def apply(a: DenseVector[T], b: SparseVector[T]): T = {
        var result: T = zero

        val bd: Array[T] = b.data
        val bi: Array[Int] = b.index
        val bsize: Int = b.iterableSize

        val adata:  Array[T] = a.data
        val aoff:   Int = a.offset
        val stride: Int = a.stride

        var i = 0
        if(stride == 1 && aoff == 0) {
          while(i < bsize) {
            result += adata(bi(i)) * bd(i)
            i += 1
          }
        } else {
          while(i < bsize) {
            result += adata(aoff + bi(i) * stride) * bd(i)
            i += 1
          }
        }
        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)
//      implicitly[BinaryRegistry[DenseVector[T], Vector[T], OpMulInner.type, T]].register(this)
    }




  @expand
  @expand.valify
  implicit def implZipValues_DVT_SVT_eq_ZVTT[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):
  zipValues.Impl2[DenseVector[T], SparseVector[T], ZippedValues[T, T]] =

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
            val svValues:  Array[T] = sv.data
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
  implicit def implScaleAdd_DVT_T_SVT_InPlace[@expand.args(Int, Double, Float, Long) T]
  (implicit  @expand.sequence[T](0, 0.0, 0f, 0l) zero: T):
  scaleAdd.InPlaceImpl3[DenseVector[T], T, SparseVector[T]] =

    new scaleAdd.InPlaceImpl3[DenseVector[T], T, SparseVector[T]] {
      def apply(y: DenseVector[T], a: T, x: SparseVector[T]): Unit = {
        require(x.length == y.length, "Vectors must be the same length!")
        val xsize: Int = x.activeSize

        if(a != zero) {

          var xoff: Int = 0
          while (xoff < xsize) {
            y(x.indexAt(xoff)) += a * x.valueAt(xoff)
            xoff += 1
          }

        }

      }
      implicitly[TernaryUpdateRegistry[Vector[T], T, Vector[T], scaleAdd.type]].register(this)
    }

  // this shouldn't be necessary but it is:
  @expand
  @expand.valify
  implicit def dv_sv_op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType] = {
    val op = DenseVector.pureFromUpdate(implicitly[Op.InPlaceImpl2[DenseVector[T], SparseVector[T]]])
    implicitly[BinaryRegistry[DenseVector[T], Vector[T], Op.type, Vector[T]]].register(op)
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(op)
  }
}


trait SparseVectorOps { this: SparseVector.type =>
  import breeze.math.PowImplicits._

  implicit def liftCSCOpToSVTransposeOp[Tag,V,LHS,R](implicit op: UFunc.UImpl2[Tag,LHS,CSCMatrix[V],R],
                                            zero: Zero[V], ct: ClassTag[V]):
  UFunc.UImpl2[Tag,LHS,Transpose[SparseVector[V]],R] =
    new UFunc.UImpl2[Tag,LHS,Transpose[SparseVector[V]],R] {
      def apply(v: LHS, v2: Transpose[SparseVector[V]]): R = {
        op(v,v2.inner.asCSCMatrix())
      }
    }

  @expand
  @expand.valify
  implicit def implOps_SVT_SVT_eq_SVT[@expand.args(Int, Double, Float, Long) T,
                                      @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}) op: Op.Impl2[T, T, T],
            @expand.sequence[T](0, 0.0, 0f, 0l) zero: T):
  Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =

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
        while(aoff < asize) {

          while(boff < bsize && b.indexAt(boff) < a.indexAt(aoff)) {
            resultI(resultOff) = b.indexAt(boff)
            resultV(resultOff) = op(q, b.valueAt(boff))
            resultOff += 1
            boff += 1
          }

          val bvalue: T = if(boff < bsize && b.indexAt(boff) == a.indexAt(aoff)) {
            val bv: T = b.valueAt(boff)
            boff += 1
            bv
          }  else {
            q
          }
          resultI(resultOff) = a.indexAt(aoff)
          resultV(resultOff) = op(a.valueAt(aoff), bvalue)
          resultOff += 1
          aoff += 1
        }

        while(boff < bsize) {
          resultI(resultOff) = b.indexAt(boff)
          resultV(resultOff) = op(q, b.valueAt(boff))
          resultOff += 1
          boff += 1
        }

        if(resultOff != resultI.length) {
          new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), util.Arrays.copyOf(resultV, resultOff), resultOff, a.length)
        } else {
          new SparseVector[T](resultI, resultV, resultOff, a.length)
        }
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }


  implicit def implSubOp_SVT_SVT_eq_SVT[T:Ring:ClassTag]: OpSub.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] = {
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
          Array.copy(resultV, 0,dat,0,resultOff)
          new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), dat, resultOff, a.length)
        } else {
          new SparseVector[T](resultI, resultV, resultOff, a.length)
        }
      }
    }
  }

  implicit def implAddOp_SVT_SVT_eq_SVT[T:Semiring:ClassTag]: OpAdd.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] = {
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
          Array.copy(resultV, 0,dat,0,resultOff)
          new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), dat, resultOff, a.length)
        } else {
          new SparseVector[T](resultI, resultV, resultOff, a.length)
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def implOpMulScalar_SVT_SVT_eq_SVT[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0f,  0l) zero: T):
  OpMulScalar.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =

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
                assert(newAoff > aoff, bind + " " + aoff + " " + newAoff + " " + a.index(aoff) + " " + a.index(newAoff) + " " + a + " " + b)
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
            new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), util.Arrays.copyOf(resultV, resultOff), resultOff, a.length)
          } else {
            new SparseVector[T](resultI, resultV, resultOff, a.length)
          }
        }
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]]].register(this)
    }



  @expand
  @expand.valify
  implicit def implOps_SVT_SVT_eq_SVT[@expand.args(Int, Double, Float, Long) T,
                                      @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] =

    new Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        var i: Int = 0
        while(i < a.length) {
          result.add(i, op(a(i), b(i)))
          i += 1
        }
        result.toSparseVector(true, true)
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }



  @expand
  @expand.valify
  implicit def implOps_SVT_VT_eq_SVT[@expand.args(Int, Double, Float, Long) T,
                                     @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.Impl2[SparseVector[T], Vector[T], SparseVector[T]] =

    new Op.Impl2[SparseVector[T], Vector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: Vector[T]): SparseVector[T] = {
        require(b.length == a.length, "Vectors must be the same length!")
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        var i: Int = 0
        while(i < a.length) {
          result.add(i, op(a(i), b(i)))
          i += 1
        }
        result.toSparseVector(true, true)
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  @expand
  implicit def implOpSVT_Field_SVT[@expand.args(OpAdd,OpSub,OpDiv,OpMod,OpPow) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f.+(_,_)}, {f.-(_,_)}, {f./(_,_)}, {f.%(_,_)},{f.pow(_,_)}) op: Op.Impl2[T,T,T]): Op.Impl2[SparseVector[T],T,SparseVector[T]] = {

    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)
        val f = implicitly[Field[T]]
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
  }

  @expand
  implicit def implOps_SVT_Field_eq_SVT[@expand.args(OpMulScalar, OpMulMatrix) Op<:OpType, T:Field:ClassTag]:
  Op.Impl2[SparseVector[T], T, SparseVector[T]] =

    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      val f = implicitly[Field[T]]
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)

        var i: Int = 0
        while(i < a.activeSize) {
          result.add(a.indexAt(i), f.*(a.valueAt(i), b))
          i += 1
        }
        result.toSparseVector(true, true)
      }
      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def implOps_SVT_T_eq_SVT[@expand.args(Int, Double, Float, Long) T,
                                    @expand.args(OpAdd, OpSub, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T],
            @expand.sequence[T](0, 0.0, 0.0f, 0l)  zero: T):
  Op.Impl2[SparseVector[T], T, SparseVector[T]] =

    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)

        var i: Int = 0
        while(i < a.length) {
          val r =  op(a(i), b)
          if(r  != zero)
            result.add(i,r)
          i += 1
        }
        result.toSparseVector(true, true)
      }
      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
    }



  @expand
  @expand.valify
  implicit def implOps_SVT_T_eq_SVT[@expand.args(Int, Double, Float, Long) T,
                                    @expand.args(OpMulScalar, OpMulMatrix) Op<:OpType]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l)
  zero: T):
  Op.Impl2[SparseVector[T], T, SparseVector[T]] =

    new Op.Impl2[SparseVector[T], T, SparseVector[T]] {
      def apply(a: SparseVector[T], b: T): SparseVector[T] = {
        val result: VectorBuilder[T] = new VectorBuilder[T](a.length)

        var i: Int = 0
        while(i < a.activeSize) {
          result.add(a.indexAt(i), a.valueAt(i) * b)
          i += 1
        }
        result.toSparseVector(true, true)
      }
      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
    }

  protected def updateFromPure[T, Op<:OpType, Other](implicit op: UFunc.UImpl2[Op, SparseVector[T], Other, SparseVector[T]]): UFunc.InPlaceImpl2[Op, SparseVector[T], Other] = {
    new UFunc.InPlaceImpl2[Op, SparseVector[T], Other] {
      def apply(a: SparseVector[T], b: Other) {
        val result = op(a, b)
        a.use(result.index, result.data, result.activeSize)
      }
    }
  }


  implicit def implOpSet_SVT_SVT_InPlace[T]: OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]] = {
    new OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]] {
      def apply(a: SparseVector[T], b: SparseVector[T]) {
        val result = b.copy
        a.use(result.index, result.data, result.activeSize)
      }
    }
  }

  implicit def implOpSet_SVT_T_InPlace[T:Semiring:ClassTag]: OpSet.InPlaceImpl2[SparseVector[T], T] = {
    val zero = implicitly[Semiring[T]].zero
    new OpSet.InPlaceImpl2[SparseVector[T], T] {
      def apply(a: SparseVector[T], b: T) {
        if(b == zero) {
          a.use(new Array[Int](2), new Array[T](2), 0)
          return
        }
        val data = Array.fill(a.length)(b)
        val index = Array.range(0, a.length)
        a.use(index, data, a.length)
      }
    }
  }

  // this shouldn't be necessary but it is:
  @expand
  @expand.valify
  implicit def implOps_SVT_SVT_InPlace[@expand.args(Int, Double, Float, Long) T,
                            @expand.args(OpAdd, OpSub, OpDiv, OpPow, OpMod, OpMulScalar) Op <: OpType]:
  Op.InPlaceImpl2[SparseVector[T], SparseVector[T]] = {
    val uop: Op.InPlaceImpl2[SparseVector[T], SparseVector[T]] = updateFromPure(implicitly[Op.Impl2[SparseVector[T], SparseVector[T], SparseVector[T]]])
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(uop)
    uop
  }

  @expand
  implicit def implOps_SVT_Field_InPlace[@expand.args(OpAdd, OpSub, OpDiv, OpPow, OpMod, OpMulScalar, OpMulMatrix) Op <: OpType, T:Field:ClassTag]:
  Op.InPlaceImpl2[SparseVector[T], T]  = {
    val uop: Op.InPlaceImpl2[SparseVector[T], T] = updateFromPure(implicitly[Op.Impl2[SparseVector[T], T, SparseVector[T]]])
    uop
  }


  @expand
  @expand.valify
  implicit def implOps_SVT_T_InPlace[@expand.args(Int, Double, Float, Long) T,
                           @expand.args(OpAdd, OpSub, OpDiv, OpPow, OpMod, OpMulScalar, OpMulMatrix) Op <: OpType]:
  Op.InPlaceImpl2[SparseVector[T], T]  = {
    val uop: Op.InPlaceImpl2[SparseVector[T], T] = updateFromPure(implicitly[Op.Impl2[SparseVector[T], T, SparseVector[T]]])
    uop
  }

  @expand
  @expand.valify
  implicit def implOpMulInner_SVT_SVT_eq_T [@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0f,  0l) zero: T):
  OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] =

    new OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] {
      def apply(a: SparseVector[T], b: SparseVector[T]): T = {
        if(b.activeSize < a.activeSize) {
          apply(b, a)
        } else {
          require(b.length == a.length, "Vectors must be the same length!")
          val asize: Int = a.activeSize
          val bsize: Int = b.activeSize

          var result: T = zero

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
                assert(newAoff > aoff, aoff + " " + newAoff)
                aoff = newAoff
              }
            } else {
              // b is there, a is there, do the multiplication!
              result += a.valueAt(aoff) * b.valueAt(boff)
              aoff += 1
              boff += 1
            }
          }

          result
        }
      }

      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner.type, T]].register(this)

    }

  implicit def implOpMulInner_SVT_SVT_eq_T [T:ClassTag:Zero:Semiring]:
  OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] =

    new OpMulInner.Impl2[SparseVector[T], SparseVector[T], T] {
      val s = implicitly[Semiring[T]]
      def apply(a: SparseVector[T], b: SparseVector[T]): T = {
        if(b.activeSize < a.activeSize) {
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
                assert(newAoff > aoff, aoff + " " + newAoff)
                aoff = newAoff
              }
            } else {
              // b is there, a is there, do the multiplication!
              result = s.+(result,s.*(a.valueAt(aoff), b.valueAt(boff)))
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
  implicit def implScaleAdd_SVT_T_SVT_InPlace[@expand.args(Int, Double, Float, Long) T]
  (implicit  @expand.sequence[T](0, 0.0, 0f, 0l) zero: T):
  scaleAdd.InPlaceImpl3[SparseVector[T], T, SparseVector[T]] =

    new scaleAdd.InPlaceImpl3[SparseVector[T], T, SparseVector[T]] {
      def apply(y: SparseVector[T], a: T, x: SparseVector[T]): Unit = {
        require(x.length == y.length, "Vectors must be the same length!")
        val asize: Int = y.activeSize
        val bsize: Int = x.activeSize

        if(a != zero) {

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

  @expand
  implicit def implScaleAdd_SVT_Field_SVT_InPlace[T:Field:ClassTag]:
  scaleAdd.InPlaceImpl3[SparseVector[T], T, SparseVector[T]] =

    new scaleAdd.InPlaceImpl3[SparseVector[T], T, SparseVector[T]] {
      val f = implicitly[Field[T]]
      def apply(y: SparseVector[T], a: T, x: SparseVector[T]): Unit = {
        require(x.length == y.length, "Vectors must be the same length!")
        val asize: Int = y.activeSize
        val bsize: Int = x.activeSize

        if(a != f.zero) {

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
              resultV(resultOff) = f.*(a, x.valueAt(boff))
              resultOff += 1
              boff += 1
            }

            val bvalue: T = if (boff < bsize && x.indexAt(boff) == y.indexAt(aoff)) {
              val bv: T = f.*(a, x.valueAt(boff))
              boff += 1
              bv
            } else {
              f.zero
            }
            resultI(resultOff) = y.indexAt(aoff)
            resultV(resultOff) = f.+(y.valueAt(aoff), bvalue)
            resultOff += 1
            aoff += 1
          }

          while (boff < bsize) {
            resultI(resultOff) = x.indexAt(boff)
            resultV(resultOff) = f.*(a,x.valueAt(boff))
            resultOff += 1
            boff += 1
          }

          if (resultOff != resultI.length) {
            y.use(util.Arrays.copyOf(resultI, resultOff), ArrayUtil.copyOf[T](resultV, resultOff), resultOff)
          } else {
            y.use(resultI, resultV, resultOff)
          }
        }
      }
    }

  implicit def implNorm_SVT_Field_eq_D[T](implicit f: Field[T]):
  norm.Impl2[SparseVector[T], Double, Double] =

    new norm.Impl2[SparseVector[T], Double, Double] {
      def apply(v: SparseVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum: Double = 0.0
          activeValuesIterator foreach (v => sum += f.sNorm(v) )
          sum
        } else if (n == 2) {
          var sum: Double = 0.0
          activeValuesIterator  foreach (v => { val nn = f.sNorm(v); sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max: Double = 0.0
          activeValuesIterator foreach (v => { val nn = f.sNorm(v); if (nn > max) max = nn })
          max
        } else {
          var sum: Double = 0.0
          activeValuesIterator foreach (v => { val nn = f.sNorm(v); sum += math.pow(nn,n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }

  @expand
  @expand.valify
  implicit def implNorm_SVT_D_eq_D[@expand.args(Int, Double, Float, Long) T]:
  norm.Impl2[SparseVector[T], Double, Double] =

    new norm.Impl2[SparseVector[T], Double, Double] {
      def apply(v: SparseVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum: Double = 0.0
          activeValuesIterator foreach (v => sum += v.abs.toDouble )
          sum
        } else if (n == 2) {
          var sum: Double = 0.0
          activeValuesIterator  foreach (v => { val nn = v.abs.toDouble; sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max: Double = 0.0
          activeValuesIterator foreach (v => { val nn = v.abs.toDouble; if (nn > max) max = nn })
          max
        } else {
          var sum: Double = 0.0
          activeValuesIterator foreach (v => { val nn = v.abs.toDouble; sum += math.pow(nn,n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }

  class CanZipMapValuesSparseVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV:ClassTag:Zero]
  extends CanZipMapValues[SparseVector[V],V,RV,SparseVector[RV]] {
    def create(length : Int): SparseVector[RV] = zeros(length)

    /**Maps all corresponding values from the two collection. */
    def map(from: SparseVector[V], from2: SparseVector[V], fn: (V, V) => RV): SparseVector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result: SparseVector[RV] = create(from.length)
      var i = 0
      while (i < from.length) {
        result(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }
  }
  implicit def zipMap[V, R:ClassTag:Zero] = new CanZipMapValuesSparseVector[V, R]
  implicit val zipMap_d: CanZipMapValuesSparseVector[Double, Double] = new CanZipMapValuesSparseVector[Double, Double]
  implicit val zipMap_f: CanZipMapValuesSparseVector[Float, Float] = new CanZipMapValuesSparseVector[Float, Float]
  implicit val zipMap_i: CanZipMapValuesSparseVector[Int, Int] = new CanZipMapValuesSparseVector[Int, Int]

  class CanZipMapKeyValuesSparseVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV:ClassTag:Zero]
    extends CanZipMapKeyValues[SparseVector[V],Int, V,RV,SparseVector[RV]] {
    def create(length : Int): SparseVector[RV] = zeros(length)

    /**Maps all corresponding values from the two collection. */
    def map(from: SparseVector[V], from2: SparseVector[V], fn: (Int, V, V) => RV): SparseVector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result: SparseVector[RV] = create(from.length)
      var i = 0
      while (i < from.length) {
        result(i) = fn(i, from(i), from2(i))
        i += 1
      }
      result
    }
  }
  implicit def zipMapKV[V, R:ClassTag:Zero] = new CanZipMapKeyValuesSparseVector[V, R]


  implicit def implOpNeg_SVT_eq_SVT[@spec(Double, Int, Float, Long)  V]
  (implicit scale: OpMulScalar.Impl2[SparseVector[V], V, SparseVector[V]], field: Ring[V]):
  OpNeg.Impl[SparseVector[V], SparseVector[V]] = {
    new OpNeg.Impl[SparseVector[V], SparseVector[V]] {
      override def apply(a : SparseVector[V]): SparseVector[V] = {
        scale(a, field.negate(field.one))
      }
    }
  }

}

trait SparseVector_DenseMatrixOps { this: SparseVector.type =>
  @expand
  @expand.valify
  implicit def implOpMulMatrix_DM_SV_eq_DV[@expand.args(Int, Float, Long, Double) T]:OpMulMatrix.Impl2[DenseMatrix[T], SparseVector[T], DenseVector[T]] = {
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
