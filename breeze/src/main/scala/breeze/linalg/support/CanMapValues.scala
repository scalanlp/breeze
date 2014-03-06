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
import breeze.linalg.support.CanMapValues.HandHold

/**
 * Marker for being able to map the keys and values in a value collection
 * to new values.
 *
 * @author dramage
 * @author dlwh
 */
trait CanMapValues[From, +A, B, +To] {
  /**Maps all key-value pairs from the given collection. */
  def map(from: From, fn: (A => B)): To

  /**Maps all active key-value pairs from the given collection. */
  def mapActive(from: From, fn: (A => B)): To
}

trait CanMapValuesLowPrio {
  implicit def handHoldFromCMV[From, ValueType](implicit cmv: CanMapValues[From, ValueType, ValueType, From]) = new HandHold[From, ValueType]

  /*implicit*/ def canMapSelf[V, V2]: CanMapValues[V, V, V2, V2] = {
    new CanMapValues[V, V, V2, V2] {
      def map(from: V, fn: (V) => V2) = fn(from)
      def mapActive(from: V, fn: (V) => V2) = fn(from)
    }
  }

}

object CanMapValues extends CanMapValuesLowPrio {
  class HandHold[From, ValueType]

  /*
  implicit def canMapSelf[V, V2]: CanMapValues[V, V, V2, V2] = {
    new CanMapValues[V, V, V2, V2] {
      def map(from: V, fn: (V) => V2) = fn(from)
      def mapActive(from: V, fn: (V) => V2) = fn(from)
    }
  }
  */

  type Op[From, A, B, To] = CanMapValues[From, A, B, To]

  //
  // Arrays
  //

  class OpArray[@specialized(Int, Float, Double) A, @specialized(Int, Float, Double) B: ClassTag]
    extends Op[Array[A], A, B, Array[B]] {

    /**Maps all values from the given collection. */
    def map(from: Array[A], fn: (A) => B): Array[B] = {
      val arr = new Array[B](from.length)
      for(i <- 0 until from.length) {
        arr(i) = fn(from(i))
      }
      arr
    }

    /**Maps all active key-value pairs from the given collection. */
    def mapActive(from: Array[A], fn: (A) => B): Array[B] = map(from, fn)
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

  implicit def handholdArray[T]:HandHold[Array[T], T] = new HandHold[Array[T], T]
}
