package breeze.polynomial

import breeze.numerics._
import breeze.generic._
import breeze.linalg.{DenseVector, DenseMatrix}
import spire.math._
import spire.math.poly._
import spire.algebra._
import spire.implicits._

trait DensePolynomial {
  object densePolyval extends UFunc {
    implicit object doubleImpl extends Impl2[PolyDenseUFuncWrapper,Double,Double] {
      def apply(k: PolyDenseUFuncWrapper, v: Double) = k.p(v)
    }
    implicit object denseVectorImpl extends Impl2[PolyDenseUFuncWrapper,DenseVector[Double],DenseVector[Double]] {
      /* This implementation uses Horner's Algorithm:
       *  http://en.wikipedia.org/wiki/Horner's_method
       *
       *  Iterating over the polynomial coefficients first and the
       *  vector coefficients second is about 3x faster than
       *  the other way around.
       */
      def apply(k: PolyDenseUFuncWrapper, v: DenseVector[Double]) = {
        val coeffs: Array[Double] = k.p.coeffs
        var i = coeffs.length - 1
        var result = DenseVector.fill[Double](v.size, coeffs(i))
        while (i > 0) {
          i -= 1
          val c = coeffs(i)
          cfor(0)(j => j < result.size, j => j+1)( j => {
            result.unsafeUpdate(j, result.unsafeValueAt(j)*v.unsafeValueAt(j)+c)
          })
        }
        result
      }
    }
    implicit object denseMatrixImpl extends Impl2[PolyDenseUFuncWrapper,DenseMatrix[Double],DenseMatrix[Double]] {
      /* This implementation uses Horner's Algorithm:
       *  http://en.wikipedia.org/wiki/Horner's_method
       *
       *  Iterating over the polynomial coefficients first and the
       *  vector coefficients second is about 3x faster than
       *  the other way around.
       */
      def apply(k: PolyDenseUFuncWrapper, v: DenseMatrix[Double]) = {
        if (v.rows != v.cols) {
          throw new IllegalArgumentException("Can only apply polynomial to square matrix.")
        }
        val n = v.rows
        val coeffs: Array[Double] = k.p.coeffs
        var i = coeffs.length - 1
        var result = DenseMatrix.eye[Double](n) * coeffs(i)
        while (i > 0) {
          i -= 1
          result = result*v //WILDLY INEFFICIENT, FIGURE OUT IN PLACE MULTIPLY
          val c = coeffs(i)
          cfor(0)(i => i < n, i => i+1)(i => {
            result.update(i,i, result(i,i)+c)
          })
        }
        result
      }
    }

  }

  implicit class PolyDenseUFuncWrapper(val p: PolyDense[Double]) extends VariableUFunc[densePolyval.type,PolyDenseUFuncWrapper]
}
