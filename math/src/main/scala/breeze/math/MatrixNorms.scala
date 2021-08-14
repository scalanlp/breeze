package breeze.math

import breeze.linalg.operators.{OpMulInner, OpMulScalar}
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.{norm, sum}
import breeze.numerics.{pow, sqrt}

/**
 * breeze
 * 7/10/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 * TODO: Probably implicits on norm methods should be removed for implementation
 * of norms that do not require CanTraverseValues.
 */
trait MatrixNorms[M, S] {
  implicit def canNorm_Int(implicit iter: CanTraverseValues[M, Int]): norm.Impl2[M, Int, Double]
  implicit def canNorm_Float(implicit iter: CanTraverseValues[M, Float]): norm.Impl2[M, Float, Double]
  implicit def canNorm_Double(implicit iter: CanTraverseValues[M, Double]): norm.Impl2[M, Double, Double]
  implicit def canNorm_Field(implicit field: Field[S]): norm.Impl2[M, Double, Double]
}

trait MatrixInnerProduct[M, S] extends MatrixNorms[M, S] {
  def innerProduct(m1: M, m2: M): S

  implicit val canInnerProduct: OpMulInner.Impl2[M, M, S] = new OpMulInner.Impl2[M, M, S] {
    override def apply(v: M, v2: M): S = innerProduct(v, v2)
  }

  implicit def canInnerProductNorm_Ring(implicit ring: Ring[S]): norm.Impl[M, Double] = new norm.Impl[M, Double] {
    override def apply(v: M): Double = sqrt(implicitly[Ring[S]].sNorm(canInnerProduct(v, v)))
  }
}

object EntrywiseMatrixNorms {
  def make[M, S](implicit field: Field[S], hadamard: OpMulScalar.Impl2[M, M, M], iter: CanTraverseValues[M, S]) =
    new MatrixInnerProduct[M, S] {

      override def innerProduct(m1: M, m2: M): S = sum(hadamard(m1, m2))

      override implicit def canNorm_Int(implicit iter: CanTraverseValues[M, Int]): norm.Impl2[M, Int, Double] =
        new norm.Impl2[M, Int, Double] {
          def apply(v: M, n: Int): Double = {

            class NormVisitor extends ValuesVisitor[Int] {
              var agg: Double = 0.0
              val (op, opEnd) =
                if (n == 1) ((v: Int) => agg += v.abs.toDouble, identity[Double] _)
                else if (n == 2) ((v: Int) => {
                  val nn = v.abs.toDouble
                  agg += nn * nn
                }, (e: Double) => sqrt(e))
                else if (n == Int.MaxValue) {
                  ((v: Int) => {
                    val nn = v.abs.toDouble
                    if (nn > agg) agg = nn
                  }, identity[Double] _)
                } else {
                  ((v: Int) => {
                    val nn = v.abs.toDouble
                    agg += pow(v, n)
                  }, (e: Double) => pow(e, 1.0 / n))
                }

              def visit(a: Int): Unit = op(a)

              def zeros(numZero: Int, zeroValue: Int): Unit = {}

              def norm = opEnd(agg)
            }

            val visit = new NormVisitor
            iter.traverse(v, visit)
            visit.norm
          }
        }

      override implicit def canNorm_Float(implicit iter: CanTraverseValues[M, Float]): norm.Impl2[M, Float, Double] =
        new norm.Impl2[M, Float, Double] {
          def apply(v: M, n: Float): Double = {

            class NormVisitor extends ValuesVisitor[Float] {
              var agg: Double = 0.0
              val (op, opEnd) =
                if (n == 1) ((v: Float) => agg += v.abs.toDouble, identity[Double] _)
                else if (n == 2) ((v: Float) => {
                  val nn = v.abs.toDouble
                  agg += nn * nn
                }, (e: Double) => sqrt(e))
                else if (n == Float.PositiveInfinity) {
                  ((v: Float) => {
                    val nn = v.abs.toDouble
                    if (nn > agg) agg = nn
                  }, identity[Double] _)
                } else {
                  ((v: Float) => {
                    val nn = v.abs.toDouble
                    agg += pow(v, n)
                  }, (e: Double) => pow(e, 1.0 / n))
                }

              def visit(a: Float): Unit = op(a)

              def zeros(numZero: Int, zeroValue: Float): Unit = {}

              def norm = opEnd(agg)
            }

            val visit = new NormVisitor
            iter.traverse(v, visit)
            visit.norm
          }
        }

      override implicit def canNorm_Double(implicit iter: CanTraverseValues[M, Double]): norm.Impl2[M, Double, Double] =
        new norm.Impl2[M, Double, Double] {
          def apply(v: M, n: Double): Double = {

            class NormVisitor extends ValuesVisitor[Double] {
              var agg: Double = 0.0
              val (op, opEnd) =
                if (n == 1) ((v: Double) => agg += v.abs, identity[Double] _)
                else if (n == 2) ((v: Double) => {
                  val nn = v.abs
                  agg += nn * nn
                }, (e: Double) => sqrt(e))
                else if (n == Double.PositiveInfinity) {
                  ((v: Double) => {
                    val nn = v.abs
                    if (nn > agg) agg = nn
                  }, identity[Double] _)
                } else {
                  ((v: Double) => {
                    val nn = v.abs
                    agg += pow(v, n)
                  }, (e: Double) => pow(e, 1.0 / n))
                }

              def visit(a: Double): Unit = op(a)

              def zeros(numZero: Int, zeroValue: Double): Unit = {}

              def norm = opEnd(agg)
            }

            val visit = new NormVisitor
            iter.traverse(v, visit)
            visit.norm
          }
        }

      override implicit def canNorm_Field(implicit field: Field[S]): norm.Impl2[M, Double, Double] =
        new norm.Impl2[M, Double, Double] {
          def apply(v: M, n: Double): Double = {

            class NormVisitor extends ValuesVisitor[S] {
              var agg: Double = 0.0
              val (op, opEnd) =
                if (n == 1) ((v: S) => agg += field.sNorm(v), identity[Double] _)
                else if (n == 2) ((v: S) => {
                  val nn = field.sNorm(v)
                  agg += nn * nn
                }, (e: Double) => sqrt(e))
                else if (n == Double.PositiveInfinity) {
                  ((v: S) => {
                    val nn = field.sNorm(v)
                    if (nn > agg) agg = nn
                  }, identity[Double] _)
                } else {
                  ((v: S) => {
                    val nn = field.sNorm(v)
                    agg += pow(nn, n)
                  }, (e: Double) => pow(e, 1.0 / n))
                }

              def visit(a: S): Unit = op(a)

              def zeros(numZero: Int, zeroValue: S): Unit = {}

              def norm = opEnd(agg)
            }

            val visit = new NormVisitor
            iter.traverse(v, visit)
            visit.norm
          }
        }
    }
}
