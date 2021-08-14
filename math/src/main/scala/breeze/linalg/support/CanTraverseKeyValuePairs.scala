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
import breeze.math.Complex
import scala.reflect.ClassTag
import breeze.linalg.support.CanTraverseKeyValuePairs.KeyValuePairsVisitor

/**
 * Marker for being able to traverse over the values in a collection/tensor
 *
 * @author dramage
 * @author dlwh
 */
trait CanTraverseKeyValuePairs[From, K, A] {

  /**Traverses all values from the given collection. */
  def traverse(from: From, fn: KeyValuePairsVisitor[K, A]): Unit
  def isTraversableAgain(from: From): Boolean
}

object CanTraverseKeyValuePairs {

  trait KeyValuePairsVisitor[@specialized(Int) K, @specialized A] {
    def visit(k: K, a: A): Unit
    def visitArray(indices: Int => K, arr: Array[A]): Unit = visitArray(indices, arr, 0, arr.length, 1)

    def visitArray(indices: Int => K, arr: Array[A], offset: Int, length: Int, stride: Int): Unit = {
      var i = 0
      while (i < length) {
        visit(indices(i * stride + offset), arr(i * stride + offset))
        i += 1
      }
    }
    def zeros(numZero: Int, zeroKeys: Iterator[K], zeroValue: A): Unit
  }

  //
  // Arrays
  //

  class OpArray[@specialized(Double, Int, Float, Long) A] extends CanTraverseKeyValuePairs[Array[A], Int, A] {

    /** Traverses all values from the given collection. */
    def traverse(from: Array[A], fn: KeyValuePairsVisitor[Int, A]): Unit = {
      fn.visitArray(0 until from.length, from)
    }

    def isTraversableAgain(from: Array[A]): Boolean = true
  }

  implicit def opArray[@specialized A]: OpArray[A] = new OpArray[A]

  implicit object OpArrayII extends OpArray[Int]

  implicit object OpArraySS extends OpArray[Short]

  implicit object OpArrayLL extends OpArray[Long]

  implicit object OpArrayFF extends OpArray[Float]

  implicit object OpArrayDD extends OpArray[Double]

  implicit object OpArrayCC extends OpArray[Complex]
}
