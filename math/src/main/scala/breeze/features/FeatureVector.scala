package breeze.features

import breeze.linalg._
import java.util
import breeze.linalg.operators._
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



  override def toString: String = data.mkString("FeatureVector(",", ", ")")

  override def hashCode = util.Arrays.hashCode(data)


  override def equals(p1: Any): Boolean = p1 match {
    case fv: FeatureVector => util.Arrays.equals(fv.data, data)
    case _ => false
  }
}

object FeatureVector {

  def apply(ints: Int*) = new FeatureVector(ints.toArray)

  object Implicits {
    implicit def fromArrayInt(arr: Array[Int]) = new FeatureVector(arr)
  }

  implicit object FVCanDaxpy extends scaleAdd.InPlaceImpl3[DenseVector[Double], Double, FeatureVector] {
    def apply(y: DenseVector[Double], a: Double, x: FeatureVector) {
      var i = 0
      while(i < x.activeLength) {
        y(x(i)) += a
        i += 1
      }
    }
  }

  implicit object FVCanDaxpyIntoVB extends scaleAdd.InPlaceImpl3[VectorBuilder[Double], Double, FeatureVector] {
    def apply(y: VectorBuilder[Double], a: Double, x: FeatureVector) {
      if(a != 0.0) {
        var i = 0
        while(i < x.activeLength) {
          y.add(x(i), a)
          i += 1
        }
      }
    }
  }

  implicit object DotProductFVDV extends OpMulInner.Impl2[FeatureVector, DenseVector[Double], Double] {
    def apply(a: FeatureVector, b: DenseVector[Double]): Double = {
      var score = 0.0
      var i = 0
      while(i < a.activeLength) {
        score += b(a(i))
        i += 1
      }

      score
    }
  }

  implicit object DotProductDVFV extends OpMulInner.Impl2[DenseVector[Double], FeatureVector, Double] {
    def apply(b: DenseVector[Double], a: FeatureVector): Double = DotProductFVDV(a, b)
  }

  implicit object FVAddIntoDV extends OpMulInner.InPlaceImpl2[DenseVector[Double], FeatureVector] {
    def apply(a: DenseVector[Double], b: FeatureVector) {
      var i = 0
      while(i < b.activeLength) {
        a(b(i)) += 1
        i += 1
      }
    }
  }

  implicit object CanMulDMFV extends OpMulMatrix.Impl2[DenseMatrix[Double], FeatureVector, DenseVector[Double]] {
    def apply(a: DenseMatrix[Double], b: FeatureVector): DenseVector[Double] = {
      val result = DenseVector.zeros[Double](a.rows)
      cforRange(0 until b.activeLength) { i =>
        result += a(::, b(i))
      }
      result
    }
  }

  implicit object CanMulCSCFV extends OpMulMatrix.Impl2[CSCMatrix[Double], FeatureVector, SparseVector[Double]] {
    def apply(a: CSCMatrix[Double], b: FeatureVector): SparseVector[Double] = {
      val result = new VectorBuilder[Double](a.rows)
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
