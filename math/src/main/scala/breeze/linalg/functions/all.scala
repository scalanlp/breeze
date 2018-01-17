package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.storage.Zero

import scala.util.control.ControlThrowable

/**
 * all(t) true if all elements of t are non-zero
 * all(f, t) returns true if all elements of t satisfy f
 *
 * @author dlwh
  **/
object all extends UFunc {
  private case object Found extends ControlThrowable

  implicit def reduceUFunc[F, T, S](
      implicit impl2: Impl2[S => Boolean, T, Boolean],
      base: UFunc.UImpl[F, S, Boolean]): Impl2[F, T, Boolean] = {
    new Impl2[F, T, Boolean] {
      override def apply(v: F, v2: T): Boolean = {
        all((x: S) => base(x), v2)
      }
    }
  }

  implicit def reduceFun[T, S](implicit ctv: CanTraverseValues[T, S]): Impl2[S => Boolean, T, Boolean] = {
    new Impl2[S => Boolean, T, Boolean] {
      override def apply(f: S => Boolean, v2: T): Boolean = {

        object Visitor extends ValuesVisitor[S] {
          def visit(a: S): Unit = {
            if (!f(a)) throw Found
          }

          def zeros(numZero: Int, zeroValue: S): Unit = {
            if (numZero != 0 && !f(zeroValue)) throw Found
          }

        }

        try {
          ctv.traverse(v2, Visitor)
          true
        } catch {
          case Found => false
        }

      }
    }
  }

  implicit def reduceZero[T, S](implicit impl2: Impl2[S => Boolean, T, Boolean], z: Zero[S]): Impl[T, Boolean] =
    new Impl[T, Boolean] {
      override def apply(v: T): Boolean = {
        all((_: S) != z.zero, v)
      }
    }

}
