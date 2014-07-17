package breeze.math

import breeze.linalg.norm
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
object MatrixNorms {

  object EntryWise {

    @expand
    implicit def canNorm[M, @expand.args(Int, Float) S](implicit iter: CanTraverseValues[M, S],
                                                        semiring: Semiring[S]): norm.Impl2[M, S, Double] = {
      new norm.Impl2[M, S, Double] {
        def apply(v: M, n: S): Double = {
          class NormVisitor extends ValuesVisitor[S] {
            var agg: Double = 0.0
            val (op, opEnd) =
              if (n == 1) ((v: S) => agg += v.abs.toDouble, identity[Double] _)
              else if (n == 2) ((v: S) => {
                val nn = v.abs.toDouble
                agg += nn * nn
              }, (e: Double) => sqrt(e))
              else if (n == Int.MaxValue || n == Float.PositiveInfinity || n == Double.PositiveInfinity) {
                ((v: S) => {
                  val nn = v.abs.toDouble
                  if (nn > agg) agg = nn
                }, identity[Double] _)
              } else {
                ((v: S) => {
                  val nn = v.abs.toDouble
                  agg += pow(v, n)
                }, (e: Double) => pow(e, 1.0 / n))
              }


            def visit(a: S): Unit = op(a)

            def zeros(numZero: Int, zeroValue: S): Unit = {
            }

            def norm = opEnd(agg)
          }

          val visit = new NormVisitor
          iter.traverse(v, visit)
          visit.norm
        }
      }
    }

    implicit def canNorm_Double[M](implicit iter: CanTraverseValues[M, Double]): norm.Impl2[M, Double, Double] = {
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
  }

}
