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
  * Marker for being able to traverse over the values in a collection/tensor.
  * Particularly useful to traverse sparse collections such as [[breeze.linalg.CSCMatrix]].
  *
  * @author dramage
  * @author dlwh
  */
trait CanTraverseKeyValuePairs[From, K, V] {

  def traverse(from: From, fn: KeyValuePairsVisitor[K, V]): Unit

  def isTraversableAgain(from: From):Boolean

}


object CanTraverseKeyValuePairs {

  trait KeyValuePairsVisitor[@specialized(Int) K, @specialized A] {
    def visit(k: K, a: A)
    def visitArray(indices: Int=>K, arr: Array[A]):Unit = visitArray(indices, arr, 0, arr.length, 1)

    def visitArray(indices: Int=>K, arr: Array[A], offset: Int, length: Int, stride: Int):Unit = {
      var i = 0
      while(i < length) {
        visit(indices(i * stride + offset), arr(i * stride + offset))
        i += 1
      }
    }
    def visitZeros(numZero: Int, zeroKeys: Iterator[K], zeroValue: A)
  }


  //
  // Arrays
  //

  class OpArray[@specialized(Double, Int, Float, Long) V]
    extends CanTraverseKeyValuePairs[Array[V], Int, V] {
    /** Traverses all values from the given collection. */
    def traverse(from: Array[V], fn: KeyValuePairsVisitor[Int, V]): Unit = {
      fn.visitArray(0 until from.length, from)
    }

    def isTraversableAgain(from: Array[V]): Boolean = true
  }


  // <editor-fold defaultstate="collapsed" desc=" implicit CanTraverseKeyValuePairs[Array[V], Int, V] implementations ">

  implicit def opArray[@specialized V] = new OpArray[V]
  implicit object OpArrayII extends OpArray[Int]
  implicit object OpArraySS extends OpArray[Short]
  implicit object OpArrayLL extends OpArray[Long]
  implicit object OpArrayFF extends OpArray[Float]
  implicit object OpArrayDD extends OpArray[Double]
  implicit object OpArrayCC extends OpArray[Complex]
}

