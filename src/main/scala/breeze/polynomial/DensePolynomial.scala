package breeze.polynomial

import breeze.numerics._
import breeze.generic._
import breeze.linalg.DenseVector
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
      def apply(k: PolyDenseUFuncWrapper, v: DenseVector[Double]) = {
        val coeffs: Array[Double] = k.p.coeffs
        var i = coeffs.length - 1
        var result = DenseVector.fill[Double](v.size, coeffs(i))
        while (i > 0) {
          i -= 1
          val c = coeffs(i)
          cfor(0)(j => j < result.size, j => j+1)( j => {
            result.unsafeUpdate(j, result.unsafeValueAt(j)*v.unsafeValueAt(j)+c)
//            result.update(j, result.valueAt(j)*v.valueAt(j)+c)
          })
        }
        result
      }
    }
  }

  implicit class PolyDenseUFuncWrapper(val p: PolyDense[Double]) extends VariableUFunc[densePolyval.type,PolyDenseUFuncWrapper]
}
