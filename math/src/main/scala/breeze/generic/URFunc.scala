package breeze.generic

import breeze.linalg.Axis
import breeze.linalg.support.CanSlice2

/**
 * A "Universal Reducer" Function that can support reduction-type operations
 * on a collection or some such.
 * At a minimum, it has to specify the three ops suppled to
 * aggregate. Other implementations may provide more efficient
 * implementations for certain common implementations of vectors and such
 *
 * @author dlwh
 */
trait URFunc[@specialized(Int, Float, Double) A, +B] {
  def apply(cc: TraversableOnce[A]):B


  def apply[T](c: T)(implicit urable: UReduceable[T, A]):B = {
    urable(c, this)
  }

  def apply[T2, Axis, TA, R](c: T2, axis: Axis)(implicit collapse: CanCollapseAxis[T2, Axis, TA, B, R], ured: UReduceable[TA, A]): R = {
    collapse(c,axis)(ta => this.apply[TA](ta))
  }

  def apply(arr: Array[A]):B = apply(arr, arr.length)
  def apply(arr: Array[A], length: Int):B = apply(arr, 0, 1, length, {_ => true})
  def apply(arr: Array[A], offset: Int, stride: Int, length: Int, isUsed: Int=>Boolean):B = {
    apply((0 until length).filter(isUsed).map(i => arr(offset + i * stride)))
  }

  def apply(as: A*):B = apply(as)

}

/**
 * An object is UReduceable (Universally Reduceable) if it can
 * deal with URFuncs in an intelligent manner.
 *
 * @author dlwh
 */
trait UReduceable[T, @specialized(Int, Float, Double)  A] extends {
  def apply[Final](c: T, f: URFunc[A, Final]):Final
}

object UReduceable {
  implicit def traversableIsUReduceable[A, T](implicit ev: T <:< Traversable[A]):UReduceable[T, A] = {
    new UReduceable[T, A] {
      def apply[Final](c: T, f: URFunc[A, Final]) = {
        f(c)
      }
    }
  }

  implicit def arrayIsUReduceable[A]:UReduceable[Array[A], A] = {
    new UReduceable[Array[A], A] {
      def apply[Final](c: Array[A], f: URFunc[A, Final]) = {
        f(c)
      }
    }
  }
}