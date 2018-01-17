package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.math.Semiring

/** Computes the product */
object product extends UFunc {

  @expand
  implicit def reduce[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl[T, S] =
    new Impl[T, S] {
      def apply(v: T): S = {
        class ProductVisitor extends ValuesVisitor[S] {
          var product: S = 1
          def visit(a: S): Unit = {
            product *= a
          }

          def zeros(numZero: Int, zeroValue: S): Unit = {
            if (numZero > 0)
              product = 0
          }
        }
        val visit = new ProductVisitor
        iter.traverse(v, visit)
        visit.product
      }
    }

  implicit def reduceSemiring[T, S](implicit iter: CanTraverseValues[T, S], semiring: Semiring[S]): Impl[T, S] =
    new Impl[T, S] {
      def apply(v: T): S = {
        class ProductVisitor extends ValuesVisitor[S] {
          var product: S = semiring.one
          def visit(a: S): Unit = {
            product = semiring.*(product, a)
          }

          def zeros(numZero: Int, zeroValue: S): Unit = {
            if (numZero > 0)
              product = semiring.zero
          }

        }
        val visit = new ProductVisitor
        iter.traverse(v, visit)
        visit.product
      }
    }
}
