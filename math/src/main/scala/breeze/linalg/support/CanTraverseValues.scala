package breeze.linalg.support

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

import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.math.Complex

/**
 * Marker for being able to traverse over the values in a breeze collection/tensor.
 * Implicit conversions to this trait allow regular scala collections and Array
 * to be traversed in the same way
 *
 * @author dramage
 * @author dlwh
 */
trait CanTraverseValues[From, A] {

  /**Traverses all values from the given breeze collection. */
  def traverse(from: From, fn: ValuesVisitor[A]): Unit

  def isTraversableAgain(from: From): Boolean

  def foldLeft[B](from: From, b: B)(fn: (B, A)=>B):B = {
    var bb = b

    traverse(from, new ValuesVisitor[A] {
      override def visit(a: A): Unit = {
        bb = fn(bb, a)
      }

      override def zeros(numZero: Int, zeroValue: A): Unit = {
        for(i <- 0 until numZero) {
          bb = fn(bb, zeroValue)
        }
      }
    })

    bb
  }
}



object CanTraverseValues {

  /**
    * This trait can be implemented and used to apply a function over a breeze collection/tensor,
    * using [[CanTraverseValues.traverse()]].
    *
    * @tparam V
    */
  trait ValuesVisitor[@specialized V] {

    def visit(a: V)

    /** Visits an array assuming offset of zero and stride of 1 */
    final def visitArray(arr: Array[V]): Unit = visitArray(arr, 0, arr.length, 1)

    def visitArray(arr: Array[V], offset: Int, length: Int, stride: Int): Unit = {
      import spire.syntax.cfor._
      // Standard array bounds check stuff
      if (stride == 1) {
        cforRange(offset until length + offset) { i => visit(arr(i)) }
      } else {
        cforRange(0 until length) { i => visit(arr(i * stride + offset)) }
      }
    }

    def zeros(numZero: Int, zeroValue: V): Unit

  }

  //
  // Arrays
  //

  class OpArray[@specialized(Double, Int, Float, Long) V]
    extends CanTraverseValues[Array[V], V] {

    /** Traverses all values from the given collection. */
    def traverse(from: Array[V], fn: ValuesVisitor[V]): Unit = {
      fn.visitArray(from)
    }

    def isTraversableAgain(from: Array[V]): Boolean = true

  }

  implicit def opArray[@specialized A] = new OpArray[A]

  /** Implicit object to apply breeze traversable operations to Array[Int].*/
  implicit object ImplicitOpArrayInt extends OpArray[Int]

  /** Implicit object to apply breeze traversable operations to Array[Short].*/
  implicit object ImplicitOpArrayShort extends OpArray[Short]

  /** Implicit object to apply breeze traversable operations to Array[Short].*/
  implicit object ImplicitOpArrayLong extends OpArray[Long]

  /** Implicit object to apply breeze traversable operations to Array[Float].*/
  implicit object ImplicitOpArrayFloat extends OpArray[Float]

  /** Implicit object to apply breeze traversable operations to Array[Double].*/
  implicit object ImplicitOpArrayDouble extends OpArray[Double]

  /** Implicit object to apply breeze traversable operations to Array[Complex].*/
  implicit object ImplicitOpArrayComplex extends OpArray[Complex]

  /**
    * Implicit conversion to apply breeze traversing operations to
    * scala collections or iterators ([[scala.TraversableOnce]] objects).
    */
  implicit def canTraverseTraversable[V,X <: TraversableOnce[V]]: CanTraverseValues[X, V] = {
    new CanTraverseValues[X, V] {

      /** Traverses all values from the given collection. */
      override def traverse(from: X, fn: CanTraverseValues.ValuesVisitor[V]): Unit = {
        for(v <- from) {
          fn.visit(v)
        }
      }

      def isTraversableAgain(from: X): Boolean = from.isTraversableAgain

    }
  }
}

@deprecated("1.0")
trait LowPrioCanTraverseValues {
  this: CanTraverseValues.type =>

  implicit def canTraverseSelf[V, V2]: CanTraverseValues[V, V] = {
    new CanTraverseValues[V, V] {

      /** Traverses all values from the given collection. */
      override def traverse(from: V, fn: CanTraverseValues.ValuesVisitor[V]): Unit = fn.visit(from)

      def isTraversableAgain(from: V): Boolean = true

    }
  }
}
