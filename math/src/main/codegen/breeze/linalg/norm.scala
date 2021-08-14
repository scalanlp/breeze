package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support.{CanTraverseValues, ScalarOf}
import breeze.macros.expand
import breeze.math.Field

/**
 * Computes the norm of an object. Many tensor objects have a norm implementation implicit, which is what this calls.
 */
object norm extends UFunc {
  @expand
  @expand.valify
  implicit def scalarNorm[@expand.args(Int, Long, Float, Double) T]: Impl[T, Double] = new Impl[T, Double] {
    def apply(v1: T): Double = v1.abs.toDouble
  }

  implicit def normalNormToNormUnit[T](implicit normImpl: Impl[T, Double]): Impl2[T, Unit, Double] = {
    new Impl2[T, Unit, Double] {
      def apply(v: T, x: Unit): Double = normImpl(v)
    }
  }

  implicit def normDoubleToNormalNorm[T](implicit normImpl: Impl2[T, Double, Double]): Impl[T, Double] = {
    new Impl[T, Double] {
      def apply(v: T): Double = normImpl(v, 2.0)
    }
  }

  implicit def fromCanNormInt[T](implicit impl: Impl2[T, Double, Double]): Impl2[T, Int, Double] = {
    new Impl2[T, Int, Double] {
      def apply(v: T, v2: Int): Double = impl(v, v2)
    }
  }

  implicit def scalarNorm[T](implicit field: Field[T]): norm.Impl[T, Double] = field.normImpl

  // only autoinstantiate for Vector likes
  implicit def canNorm[Vec <: Vector[T], T](implicit canTraverseValues: CanTraverseValues[Vec, T],
                                            canNormS: norm.Impl[T, Double]): norm.Impl2[Vec, Double, Double] = {
    fromTraverseValues(canTraverseValues, canNormS)
  }

  // TODO: make a registry, make efficient 0 and 2 norm variants for core vector types
  def fromTraverseValues[Vec, T](implicit canTraverseValues: CanTraverseValues[Vec, T],
                                 canNormS: norm.Impl[T, Double]) = {
    new norm.Impl2[Vec, Double, Double] {
      def apply(vec: Vec, n: Double): Double = {
        if (n == Double.PositiveInfinity) {
          object infiniteNormVisitor extends ValuesVisitor[T] {
            var max = 0.0

            override def visit(a: T): Unit = {
              val nn = canNormS(a)
              max = scala.math.max(nn, max)
            }

            override def zeros(numZero: Int, zeroValue: T): Unit = {
              val ns = canNormS(zeroValue)
              max = scala.math.max(ns, max)
            }
          }
          canTraverseValues.traverse(vec, infiniteNormVisitor).max
        } else if (n == 0) {
          object zeroNormVisitor extends ValuesVisitor[T] {
            var nnz = 0.0

            override def visit(a: T): Unit = {
              val nn = canNormS(a)
              if (nn != 0) {
                nnz += 1
              }
            }

            override def zeros(numZero: Int, zeroValue: T): Unit = {
            }
          }
          canTraverseValues.traverse(vec, zeroNormVisitor).nnz
        } else {
          // TODO: does this need to be more stable?
          object finiteNormVisitor extends ValuesVisitor[T] {
            var sum = 0.0

            override def visit(a: T): Unit = {
              val nn = canNormS(a)
              sum += math.pow(nn, n)
            }

            override def zeros(numZero: Int, zeroValue: T): Unit = {
              val ns = canNormS(zeroValue)
              if (numZero != 0 && ns != 0.0) {
                this.sum += numZero * math.pow(ns, n)
              }
            }
          }
          val sum = canTraverseValues.traverse(vec, finiteNormVisitor).sum
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

}
