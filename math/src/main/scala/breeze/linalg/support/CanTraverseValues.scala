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

//ToDo convert to UFuncs, leave type aliases for compatibility (as was done for CanMapValues)
//see breeze.linalg.support.package.scala
/**
 * Marker for being able to traverse over the values in a collection/tensor.
 *
 * @author dramage
 * @author dlwh
 */
trait CanTraverseValues[From, A] {

  /** Traverses all values from the given breeze collection. */
  def traverse(from: From, fn: ValuesVisitor[A]): Unit

  /** Whether a breeze collection can be transversed multiple times.
    * For native breeze objects, this is mostly (?completely) true,
    * but for scala collections implicitly used as iterators, this can be false.
    * Default implementation in [[CanTraverseValues]] is true.
    *
    * @see [[CanTraverseValues.canTraverseTraversable]]
    */
  def isTraversableAgain(from: From): Boolean = true

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

  trait ValuesVisitor[@specialized A] {

    def visit(a: A)
    def visitArray(arr: Array[A]):Unit = visitArray(arr, 0, arr.length, 1)

    def visitArray(arr: Array[A], offset: Int, length: Int, stride: Int):Unit = {
      import spire.syntax.cfor._
      // Standard array bounds check stuff
      if (stride == 1) {
        cforRange(offset until length + offset) { i =>
          visit(arr(i))
        }
      } else {
        cforRange(0 until length) { i =>
          visit(arr(i * stride + offset))
        }
      }
    }
    def zeros(numZero: Int, zeroValue: A)
  }

  //
  // Arrays
  //

  class OpArray[@specialized(Double, Int, Float, Long) A]
    extends CanTraverseValues[Array[A], A] {

    def traverse(from: Array[A], fn: ValuesVisitor[A]): Unit = {
      fn.visitArray(from)
    }

    //def isTraversableAgain(from: Array[A]): Boolean = true

  }


  implicit def opArray[@specialized A] = new OpArray[A]

  implicit object OpArrayII extends OpArray[Int]

  implicit object OpArraySS extends OpArray[Short]

  implicit object OpArrayLL extends OpArray[Long]

  implicit object OpArrayFF extends OpArray[Float]

  implicit object OpArrayDD extends OpArray[Double]

  implicit object OpArrayCC extends OpArray[Complex]

  /**Implicit implementation which allows traversal of
    * scala collection objects implementing [[scala.collection.TraversableOnce]]. */
  implicit def canTraverseTraversable[V,X <: TraversableOnce[V]]: CanTraverseValues[X, V] = {
    new CanTraverseValues[X, V] {

      /** Traverses all values from the given collection. */
      override def traverse(from: X, fn: CanTraverseValues.ValuesVisitor[V]): Unit = {
        for(v <- from) {
          fn.visit(v)
        }
      }

      override def isTraversableAgain(from: X): Boolean = from.isTraversableAgain
    }
  }
}

trait LowPrioCanTraverseValues { this: CanTraverseValues.type =>
  implicit def canTraverseSelf[V, V2]: CanTraverseValues[V, V] = {
    new CanTraverseValues[V, V] {
      /** Traverses all values from the given collection. */
      override def traverse(from: V, fn: CanTraverseValues.ValuesVisitor[V]): Unit = {
        fn.visit(from)
      }


      override def isTraversableAgain(from: V): Boolean = true
    }
  }


}
