package breeze.linalg

import breeze.linalg.support.CanTraverseValues
import breeze.math.Semiring
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.generic.UFunc

/**
 * Returns true if any element is non-zero
 *
 * @author dlwh
 **/
object any extends UFunc {
  private case object Done extends Exception
  implicit def reduceSemiring[T, S](implicit iter: CanTraverseValues[T, S], semiring: Semiring[S]): Impl[T, Boolean] = new Impl[T, Boolean] {
    def apply(v: T): Boolean = {
      class SumVisitor extends ValuesVisitor[S] {
        val zero = semiring.zero
        def visit(a: S): Unit = {
          if(a != zero) throw Done
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
        }

      }
      val visit = new SumVisitor
      try {
        iter.traverse(v, visit)
        false
      } catch {
        case Done => true
      }
    }
  }

}

/**
 * Returns true if all elements are non-zero
 *
 * @author dlwh
 **/
object all extends UFunc {
  private case object Done extends Exception

  implicit def reduceSemiring[T, S](implicit iter: CanTraverseValues[T, S], semiring: Semiring[S]): Impl[T, Boolean] = new Impl[T, Boolean] {
    def apply(v: T): Boolean = {
      class SumVisitor extends ValuesVisitor[S] {
        val zero = semiring.zero
        def visit(a: S): Unit = {
          if(a == zero) throw Done
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          if(numZero != 0) throw Done
        }

      }
      val visit = new SumVisitor
      try {
        iter.traverse(v, visit)
        true
      } catch {
        case Done => false
      }
    }
  }

}

