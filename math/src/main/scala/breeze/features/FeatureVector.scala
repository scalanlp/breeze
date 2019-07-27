package breeze.features

import breeze.generic.UFunc.InPlaceImpl3
import breeze.linalg._
import java.util
import breeze.linalg.operators._
import breeze.linalg.support.CanTranspose
import breeze.macros.expand
import breeze.math.Semiring
import spire.syntax.cfor._

/**
 * Represents a feature vector of indicator (i.e. binary) features.
 * Not a full vector. Only supports dot and addition with a real vector.
 *
 * TODO: possibly rename to IndicatorVector?
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
  implicit def FVScaleAddIntoV[@expand.args(Int, Float, Double) T]
    : TernaryUpdateRegistry[Vector[T], T, FeatureVector, scaleAdd.type] = {
    new TernaryUpdateRegistry[Vector[T], T, FeatureVector, scaleAdd.type] {
      override def bindingMissing(y: Vector[T], a: T, x: FeatureVector): Unit = {
        if (a != 0.0) {
          var i = 0
          while (i < x.activeLength) {
            y(x(i)) += a
            i += 1
          }
        }
      }
    }
  }
  @expand
  @expand.valify
  implicit def FVScaleAddIntoDV[@expand.args(Int, Float, Double) T]
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
  implicit def FVCanDaxpyIntoVB[@expand.args(Float, Double) T]
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

  implicit def FVCanDaxpyIntoVB_Generic[T]: InPlaceImpl3[scaleAdd.type, VectorBuilder[T], T, FeatureVector] = {
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

  @expand
  @expand.valify
  implicit def FVScaleAddIntoSV[@expand.args(Int, Float, Double) T]
    : scaleAdd.InPlaceImpl3[SparseVector[T], T, FeatureVector] = {
    new scaleAdd.InPlaceImpl3[SparseVector[T], T, FeatureVector] {
      def apply(y: SparseVector[T], a: T, x: FeatureVector): Unit = {
        if (a != 0.0) {
          var i = 0
          while (i < x.activeLength) {
            y(x(i)) += a
            i += 1
          }
        }
      }
      implicitly[TernaryUpdateRegistry[Vector[T], T, FeatureVector, scaleAdd.type]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def DotProductFVV[@expand.args(Int, Float, Double) T]
    : BinaryRegistry[FeatureVector, Vector[T], OpMulInner.type, T] = {
    new BinaryRegistry[FeatureVector, Vector[T], OpMulInner.type, T] {
      override def bindingMissing(a: FeatureVector, b: Vector[T]): T = {
        var score: T = 0
        var i = 0
        while (i < a.activeLength) {
          score += b(a(i))
          i += 1
        }

        score
      }
    }
  }
  @expand
  @expand.valify
  implicit def DotProductFVDV[@expand.args(Int, Float, Double) T]
    : OpMulInner.Impl2[FeatureVector, DenseVector[T], T] = {
    new OpMulInner.Impl2[FeatureVector, DenseVector[T], T] {
      def apply(a: FeatureVector, b: DenseVector[T]): T = {
        var score: T = 0
        var i = 0
        while (i < a.activeLength) {
          score += b(a(i))
          i += 1
        }

        score
      }
    }
  }

  implicit def dotProductVxFVfromFVxV[V, T](
      implicit op: OpMulInner.Impl2[FeatureVector, V, T]): OpMulInner.Impl2[V, FeatureVector, T] = {
    new OpMulInner.Impl2[V, FeatureVector, T] {
      def apply(b: V, a: FeatureVector): T = op(a, b)
    }
  }

  implicit def opAddIntoFromAxpy[V, T](
      implicit op: scaleAdd.InPlaceImpl3[V, T, FeatureVector],
      semiring: Semiring[T]): OpAdd.InPlaceImpl2[V, FeatureVector] = {
    new OpAdd.InPlaceImpl2[V, FeatureVector] {
      override def apply(v: V, v2: FeatureVector): Unit = {
        op(v, semiring.one, v2)
      }
    }
  }

  @expand
  @expand.valify
  implicit def CanMulDMFV[@expand.args(Int, Double, Float) T]
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
  implicit def CanMulCSCFV[@expand.args(Int, Double, Float) T]
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

  implicit def mulMatrixTrans[M, MTrans, MulResult, MRTrans](
      implicit trans: CanTranspose[M, MTrans],
      mul: OpMulMatrix.Impl2[MTrans, FeatureVector, MulResult],
      mrTrans: CanTranspose[MulResult, MRTrans]): OpMulMatrix.Impl2[Transpose[FeatureVector], M, MRTrans] = {
    new OpMulMatrix.Impl2[Transpose[FeatureVector], M, MRTrans] {
      override def apply(v: Transpose[FeatureVector], v2: M): MRTrans = {
        mrTrans(mul(trans(v2), v.inner))
      }
    }
  }

}
