package breeze.features

import breeze.linalg._
import java.util
import breeze.linalg.operators._

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

  object Implicits {
    implicit def fromArrayInt(arr: Array[Int]) = new FeatureVector(arr)
  }

  implicit object FVCanDaxpy extends CanAxpy[Double, FeatureVector, DenseVector[Double]] {
    def apply(a: Double, x: FeatureVector, y: DenseVector[Double]) {
      var i = 0
      while(i < x.activeLength) {
        y(x(i)) += a
        i += 1
      }
    }
  }

  implicit object DotProductFVDV extends BinaryOp[FeatureVector, DenseVector[Double], OpMulInner, Double] {
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

  implicit object DotProductDVFV extends BinaryOp[DenseVector[Double], FeatureVector, OpMulInner, Double] {
    def apply(b: DenseVector[Double], a: FeatureVector): Double = DotProductFVDV(a, b)
  }

  implicit object FVAddIntoDV extends BinaryUpdateOp[DenseVector[Double], FeatureVector, OpMulInner] {
    def apply(a: DenseVector[Double], b: FeatureVector) {
      var i = 0
      while(i < b.activeLength) {
        a(b(i)) += 1
        i += 1
      }
    }
  }

  implicit object CanMulDMFV extends BinaryOp[DenseMatrix[Double], FeatureVector, OpMulMatrix, DenseVector[Double]] {
    def apply(a: DenseMatrix[Double], b: FeatureVector): DenseVector[Double] = {
      a(*, ::) dot b
    }
  }

}
