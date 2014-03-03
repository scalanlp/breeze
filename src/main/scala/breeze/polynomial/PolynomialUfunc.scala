package breeze.polynomial

import breeze.numerics._
import breeze.generic._
import breeze.linalg.DenseVector
import spire.math._
import spire.math.poly._
import spire.algebra._
import spire.implicits._

object PolyDenseUFuncWrapper {
  def apply(coeffs: Array[Double]) = new PolyDenseUFuncWrapper(Polynomial.dense(coeffs))
  def apply(poly: PolyDense[Double]) = new PolyDenseUFuncWrapper(poly)

  implicit object doubleImpl extends ImplProvider.UImplCls[PolyDenseUFuncWrapper,Double,Double] {
    def apply(k: PolyDenseUFuncWrapper, v: Double) = k.p(v)
  }
}

class PolyDenseUFuncWrapper(protected val p: PolyDense[Double]) extends UFuncCls[PolyDenseUFuncWrapper] {
}
