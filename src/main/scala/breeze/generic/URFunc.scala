package breeze.generic
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.collection.mutable.SparseArray
import breeze.linalg.support.CanZipMapValues
import breeze.linalg.Axis

/**
 * A "Universal Reducer" Function that can support reduction-type operations
 * on a collection or some such.
 * At a minimum, it has to specify the three ops suppled to
 * aggregate. Other implementations may provide more efficient
 * implementations for certain common implementations of vectors and such
 *
 * @author dlwh
 */
trait URFunc[@specialized(Int, Float, Double) A, B] {
  def apply(cc: TraversableOnce[A]):B


  def apply[T](c: T)(implicit urable: UReduceable[T, A]):B = {
    urable(c, this)
  }

  def apply[T2, AxisT<:Axis, TA, R](c: T2, axis: AxisT)(implicit collapse: CanCollapseAxis[T2, AxisT, TA, B, R], ured: UReduceable[TA, A]): R = {
    collapse(c,axis)(ta => this.apply[TA](ta))
  }

  def apply(arr: Array[A]):B = apply(arr, arr.length)
  def apply(arr: Array[A], length: Int):B = apply(arr, 0, 1, length, {_ => true})
  def apply(arr: Array[A], offset: Int, stride: Int, length: Int, isUsed: Int=>Boolean):B = {
    apply((0 until length).filter(isUsed).map(i => arr(offset + i * stride)))
  }

  def apply(as: A*):B = apply(as)
  def apply(a: A, a2: A):B = apply(Seq(a,a2):_*)

  def apply[FromT, ToT, X>:B](first: FromT, second: FromT)(implicit zipmap: CanZipMapValues[FromT, A, X, ToT]): ToT = {
    zipmap.map(first, second, apply(_:A, _:A))
  }

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

  implicit def sparseArrayIsUReduceable[A]:UReduceable[SparseArray[A], A] = {
    new UReduceable[SparseArray[A], A] {
      def apply[Final](c: SparseArray[A], f: URFunc[A, Final]) = {
        f(c.data, c.activeSize)
      }
    }
  }
}