package breeze.linalg

import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.generic.UFunc
import breeze.storage.Zero

import scala.util.control.ControlThrowable

/**
 * any(t) true if any element of t is non-zero
 * any(f, t) returns true if any element of t satisfies f
 *
 *
 * @author dlwh
  **/
object any extends UFunc {
  private case object Found extends ControlThrowable

  implicit def reduceUFunc[F, T, S](implicit
      impl2: Impl2[S => Boolean, T, Boolean],
      base: UFunc.UImpl[F, S, Boolean]
  ): Impl2[F, T, Boolean] = {
    new Impl2[F, T, Boolean] {
      override def apply(v: F, v2: T): Boolean = {
        any((x: S) => base(x), v2)
      }
    }
  }

  implicit def reduceFun[T, S](implicit ctv: CanTraverseValues[T, S]): Impl2[S => Boolean, T, Boolean] = {
    new Impl2[S => Boolean, T, Boolean] {
      override def apply(f: S => Boolean, v2: T): Boolean = {

        object Visitor extends ValuesVisitor[S] {
          def visit(a: S): Unit = {
            if (f(a)) throw Found
          }

          def zeros(numZero: Int, zeroValue: S): Unit = {
            if (numZero != 0 && f(zeroValue)) throw Found
          }

        }

        try {
          ctv.traverse(v2, Visitor)
          false
        } catch {
          case Found => true
        }

      }
    }
  }

  implicit def reduceZero[T, S](implicit impl2: Impl2[S => Boolean, T, Boolean], z: Zero[S]): Impl[T, Boolean] =
    new Impl[T, Boolean] {
      override def apply(v: T): Boolean = {
        any((_: S) != z.zero, v)
      }
    }

}
