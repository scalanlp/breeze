package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.math.Semiring

object sum extends UFunc {

  @expand
  implicit def reduce[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      class SumVisitor extends ValuesVisitor[S] {
        var sum : S = 0
        def visit(a: S): Unit = {
          sum += a
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          sum += numZero * zeroValue
        }
      }
      val visit = new SumVisitor
      iter.traverse(v, visit)
      visit.sum
    }
  }

  implicit def reduceSemiring[T, S](implicit iter: CanTraverseValues[T, S], semiring: Semiring[S]): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      class SumVisitor extends ValuesVisitor[S] {
        var sum : S = semiring.zero
        def visit(a: S): Unit = {
          sum = semiring.+(sum, a)
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
        }

      }
      val visit = new SumVisitor
      iter.traverse(v, visit)
      visit.sum
    }
  }
}
