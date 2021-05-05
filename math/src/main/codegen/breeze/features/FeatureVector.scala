package breeze.features

import breeze.generic.UFunc.InPlaceImpl3
import breeze.linalg._
import breeze.linalg.operators._
import breeze.linalg.support.CanTranspose
import breeze.macros.expand
import breeze.math.Semiring
import breeze.macros._

import java.util

/**
 * Represents a feature vector of indicator (i.e. binary) features.
 * Not a full vector. Only supports dot and addition with a real vector.
 *
 *
 * @author dlwh
 */
class FeatureVector(val data: Array[Int]) extends NumericOps[FeatureVector] {

  def repr: FeatureVector = this

  /**
   * same as data.length
   * @return
   */
  def activeLength = data.length

  /**
   * same as data(i)
   * @param i
   * @return
   */
  def apply(i: Int) = data(i)

  override def toString: String = data.mkString("FeatureVector(", ", ", ")")

  override def hashCode = util.Arrays.hashCode(data)

  override def equals(p1: Any): Boolean = p1 match {
    case fv: FeatureVector => util.Arrays.equals(fv.data, data)
    case _ => false
  }
}

object FeatureVector {

  def apply(ints: Int*) = new FeatureVector(ints.toArray)

  object Implicits {
    implicit def fromArrayInt(arr: Array[Int]): FeatureVector = new FeatureVector(arr)
  }

  implicit object TransposeFV extends CanTranspose[FeatureVector, Transpose[FeatureVector]] {
    override def apply(from: FeatureVector): Transpose[FeatureVector] = {
      new Transpose(from)
    }
  }

  @expand
  @expand.valify
  implicit def impl_scaleAdd_InPlace_V_S_FV[@expand.args(Int, Float, Double) T]
    : TernaryUpdateRegistry[Vector[T], T, FeatureVector, scaleAdd.type] = {
    new TernaryUpdateRegistry[Vector[T], T, FeatureVector, scaleAdd.type] {
      override def bindingMissing(y: Vector[T], a: T, x: FeatureVector): Unit = {
        if (a != 0.0) {
          cforRange (0 until x.activeLength) { i =>
            y(x(i)) += a
          }
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def impl_scaleAdd_InPlace_DV_S_FV[@expand.args(Int, Float, Double) T]
    : scaleAdd.InPlaceImpl3[DenseVector[T], T, FeatureVector] = {
    new scaleAdd.InPlaceImpl3[DenseVector[T], T, FeatureVector] {
      def apply(y: DenseVector[T], a: T, x: FeatureVector): Unit = {
        var i = 0
        while (i < x.activeLength) {
          y(x(i)) += a
          i += 1
        }
      }
      implicitly[TernaryUpdateRegistry[Vector[T], T, FeatureVector, scaleAdd.type]].register(this)
    }

  }

  // specialzied doesn't work here, so we're expanding
  @expand
  @expand.valify
  implicit def impl_scaleAdd_InPlace_VB_S_FV[@expand.args(Float, Double) T]
    : InPlaceImpl3[scaleAdd.type, VectorBuilder[T], T, FeatureVector] = {
    new scaleAdd.InPlaceImpl3[VectorBuilder[T], T, FeatureVector] {
      def apply(y: VectorBuilder[T], a: T, x: FeatureVector): Unit = {
        if (a != 0.0) {
          var i = 0
          while (i < x.activeLength) {
            y.add(x(i), a)
            i += 1
          }
        }
      }
    }
  }

  implicit def impl_scaleAdd_InPlace_VB_S_FV_Generic[T]: InPlaceImpl3[scaleAdd.type, VectorBuilder[T], T, FeatureVector] = {
    new scaleAdd.InPlaceImpl3[VectorBuilder[T], T, FeatureVector] {
      def apply(y: VectorBuilder[T], a: T, x: FeatureVector): Unit = {
        if (a != 0.0) {
          cforRange (0 until x.activeLength) { i =>
            y.add(x(i), a)
          }
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def impl_OpMulInner_FV_V_eq_S[@expand.args(Int, Float, Double) T]: OpMulInner.Impl2[FeatureVector, Vector[T], T] = {
    new OpMulInner.Impl2[FeatureVector, Vector[T], T] {
      override def apply(a: FeatureVector, b: Vector[T]): T = {
        var score: T = 0
        cforRange (0 until a.activeLength) { i =>
          score += b(a(i))
        }
        score
      }
    }
  }

  @expand
  @expand.valify
  implicit def impl_OpMulInner_FV_DV_eq_S[@expand.args(Int, Float, Double) T]
    : OpMulInner.Impl2[FeatureVector, DenseVector[T], T] = {
    new OpMulInner.Impl2[FeatureVector, DenseVector[T], T] {
      def apply(a: FeatureVector, b: DenseVector[T]): T = {
        var score: T = 0
        cforRange (0 until a.activeLength) { i =>
          score += b(a(i))
        }

        score
      }
    }
  }

  implicit def impl_OpMulInner_FV_T_eq_S_from_T_FV[V, T](
      implicit op: OpMulInner.Impl2[FeatureVector, V, T]): OpMulInner.Impl2[V, FeatureVector, T] = {
    new OpMulInner.Impl2[V, FeatureVector, T] {
      def apply(b: V, a: FeatureVector): T = op(a, b)
    }
  }

  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_DM_FV_eq_DV[@expand.args(Int, Double, Float) T]
    : OpMulMatrix.Impl2[DenseMatrix[T], FeatureVector, DenseVector[T]] = {
    new OpMulMatrix.Impl2[DenseMatrix[T], FeatureVector, DenseVector[T]] {
      def apply(a: DenseMatrix[T], b: FeatureVector): DenseVector[T] = {
        val result = DenseVector.zeros[T](a.rows)
        cforRange(0 until b.activeLength) { i =>
          result += a(::, b(i))
        }
        result
      }
    }
  }

  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_CSC_FV_eq_CSC[@expand.args(Int, Double, Float) T]
    : OpMulMatrix.Impl2[CSCMatrix[T], FeatureVector, SparseVector[T]] = {
    new OpMulMatrix.Impl2[CSCMatrix[T], FeatureVector, SparseVector[T]] {
      def apply(a: CSCMatrix[T], b: FeatureVector): SparseVector[T] = {
        val result = new VectorBuilder[T](a.rows)
        cforRange(0 until b.activeLength) { i =>
          val column = b(i)
          cforRange(a.colPtrs(column) until a.colPtrs(column + 1)) { off =>
            result.add(a.rowIndices(off), a.data(off))
          }
        }
        result.toSparseVector
      }
    }
  }

  // TODO: shouldn't be necessary. verify
//  implicit def mulMatrixTrans[M, MTrans, MulResult, MRTrans](
//      implicit trans: CanTranspose[M, MTrans],
//      mul: OpMulMatrix.Impl2[MTrans, FeatureVector, MulResult],
//      mrTrans: CanTranspose[MulResult, MRTrans]): OpMulMatrix.Impl2[Transpose[FeatureVector], M, MRTrans] = {
//    new OpMulMatrix.Impl2[Transpose[FeatureVector], M, MRTrans] {
//      override def apply(v: Transpose[FeatureVector], v2: M): MRTrans = {
//        mrTrans(mul(trans(v2), v.inner))
//      }
//    }
//  }

}
