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
import breeze.math.Complex
import scala.reflect.ClassTag
import breeze.generic.CanTraverseValues.ValuesVisitor

/**
 * Marker for being able to traverse over the values in a collection/tensor
 *
 * @author dramage
 * @author dlwh
 */
trait CanTraverseValues[From, A] {
  /**Traverses all values from the given collection. */
  def traverse(from: From, fn: ValuesVisitor[A]): Unit
  def onePass(from: From):Boolean = true
}



object CanTraverseValues {

  trait ValuesVisitor[@specialized A] {
    def visit(a: A)
    def visitArray(arr: Array[A]):Unit = visitArray(arr, 0, arr.length, 1)

    def visitArray(arr: Array[A], offset: Int, length: Int, stride: Int):Unit = {
      var i = 0
      while(i < length) {
        visit(arr(i * stride + offset))
        i += 1
      }
    }
    def zeros(numZero: Int, zeroValue: A)
  }


  implicit def canTraverseSelf[V, V2]: CanTraverseValues[V, V] = {
    new CanTraverseValues[V, V] {
      /** Traverses all values from the given collection. */
      def traverse(from: V, fn: ValuesVisitor[V]): Unit = {
        fn.visit(from)
      }
    }
  }

  implicit def canTraverseTraversable[X, V](implicit cast:  X<:<TraversableOnce[V]): CanTraverseValues[X, V] = {
    new CanTraverseValues[X, V] {
      /** Traverses all values from the given collection. */
      def traverse(from: X, fn: ValuesVisitor[V]): Unit = {
        for(v <- from) {
          fn.visit(v)
        }
      }

      override def onePass(from: X): Boolean = !from.isTraversableAgain
    }
  }



  //
  // Arrays
  //

  class OpArray[@specialized(Int, Float, Double) A, @specialized(Int, Float, Double) B: ClassTag]
    extends CanTraverseValues[Array[A], A] {
    /** Traverses all values from the given collection. */
    def traverse(from: Array[A], fn: ValuesVisitor[A]): Unit = {
      fn.visitArray(from)
    }
  }


  implicit def opArray[@specialized A, @specialized B: ClassTag] =
    new OpArray[A, B]

  implicit object OpArrayII extends OpArray[Int, Int]

  implicit object OpArraySS extends OpArray[Short, Short]

  implicit object OpArrayLL extends OpArray[Long, Long]

  implicit object OpArrayFF extends OpArray[Float, Float]

  implicit object OpArrayDD extends OpArray[Double, Double]

  implicit object OpArrayCC extends OpArray[Complex, Complex]

  implicit object OpArrayID extends OpArray[Int, Double]

  implicit object OpArraySD extends OpArray[Short, Double]

  implicit object OpArrayLD extends OpArray[Long, Double]

  implicit object OpArrayFD extends OpArray[Float, Double]
}
