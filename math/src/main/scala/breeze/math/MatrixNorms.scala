package breeze.math

import breeze.linalg.{sum, norm}
import breeze.linalg.operators.{OpMulInner, OpMulScalar}
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand
import breeze.numerics.{sqrt, pow}

import scala.math.BigInt

/**
 * breeze
 * 7/10/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
object FrobeniusMatrixInnerProductNorms {

  def makeMatrixNorms[M,S] = new {
    @expand
    implicit def canNorm[@expand.args(Int, Float) ST](implicit iter: CanTraverseValues[M, ST],
                                                        semiring: Semiring[ST]): norm.Impl2[M, ST, Double] = {
      new norm.Impl2[M, ST, Double] {
        def apply(v: M, n: ST): Double = {
          class NormVisitor extends ValuesVisitor[ST] {
            var agg: Double = 0.0
            val (op, opEnd) =
              if (n == 1) ((v: ST) => agg += v.abs.toDouble, identity[Double] _)
              else if (n == 2) ((v: ST) => {
                val nn = v.abs.toDouble
                agg += nn * nn
              }, (e: Double) => sqrt(e))
              else if (n == Int.MaxValue || n == Float.PositiveInfinity || n == Double.PositiveInfinity) {
                ((v: ST) => {
                  val nn = v.abs.toDouble
                  if (nn > agg) agg = nn
                }, identity[Double] _)
              } else {
                ((v: ST) => {
                  val nn = v.abs.toDouble
                  agg += pow(v, n)
                }, (e: Double) => pow(e, 1.0 / n))
              }


            def visit(a: ST): Unit = op(a)

            def zeros(numZero: Int, zeroValue: ST): Unit = {
            }

            def norm = opEnd(agg)
          }

          val visit = new NormVisitor
          iter.traverse(v, visit)
          visit.norm
        }
      }
    }

    implicit def canNorm_Double(implicit iter: CanTraverseValues[M, Double]): norm.Impl2[M, Double, Double] = {
      new norm.Impl2[M, Double, Double] {
        def apply(v: M, n: Double): Double = {
          class NormVisitor extends ValuesVisitor[Double] {
            var agg: Double = 0.0
            val (op, opEnd) =
              if (n == 1) ((v: Double) => agg += v.abs, identity[Double] _)
              else if (n == 2) ((v: Double) => {
                val nn = v.abs.toDouble
                agg += nn * nn
              }, (e: Double) => sqrt(e))
              else if (n == Int.MaxValue || n == Float.PositiveInfinity || n == Double.PositiveInfinity) {
                ((v: Double) => {
                  val nn = v.abs.toDouble
                  if (nn > agg) agg = nn
                }, identity[Double] _)
              } else {
                ((v: Double) => {
                  val nn = v.abs.toDouble
                  agg += pow(v, n)
                }, (e: Double) => pow(e, 1.0 / n))
              }


            def visit(a: Double): Unit = op(a)

            def zeros(numZero: Int, zeroValue: Double): Unit = {
            }

            def norm = opEnd(agg)
          }
          val visit = new NormVisitor
          iter.traverse(v, visit)
          visit.norm
        }
      }
    }

    implicit def canNorm(implicit iter: CanTraverseValues[M, S],
                               field: Field[S]): norm.Impl2[M, Double, Double] = {
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
              else if (n == Int.MaxValue || n == Float.PositiveInfinity || n == Double.PositiveInfinity) {
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

    implicit def canFrobInnerProduct(implicit hadamard: OpMulScalar.Impl2[M, M, M],
                                           semiring: Semiring[S], iter: CanTraverseValues[M, S]) =
      new OpMulInner.Impl2[M, M, S] {
        override def apply(v: M, v2: M): S = {
          sum(hadamard(v, v2))
        }
      }

    implicit def canFrobNorm(implicit hadamard: OpMulScalar.Impl2[M, M, M],
                                   ring: Ring[S],
                                   iter: CanTraverseValues[M, S]) =
      new norm.Impl[M, Double] {
        override def apply(v: M): Double = ring.sNorm(sum(hadamard(v, v)))
      }
  }
}
