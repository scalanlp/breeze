package breeze.linalg.support

/*
 *
 *  Copyright 2015 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

import breeze.generic.UFunc
import breeze.math.Complex
import breeze.macros.cforRange

import scala.reflect.ClassTag
import scala.{specialized => spec}

/**
 * UFunc for being able to map the keys and values in a value collection
 * to new values.
 *
 * @author dramage
 * @author dlwh
 */
trait CanMapValues[From, @specialized(Int, Float, Long, Double) V, @specialized(Int, Float, Long, Double) V2, +To] {
//  def apply(from: From, fn: V=>V2): To = map(from, fn)
  def map(from: From, fn: V => V2): To
  def mapActive(from: From, fn: V=>V2): To
}

object CanMapValues extends CanMapValuesLowPrio {

  trait DenseCanMapValues[From, V, V2, To] extends CanMapValues[From, V, V2, To] {
    final def mapActive(from: From, fn: V=>V2): To = map(from, fn)
  }

  implicit def canMapSelfDouble[V2]: CanMapValues[Double, Double, V2, V2] = canMapSelf[Double, V2]
  implicit def canMapSelfInt[V2]: CanMapValues[Int, Int, V2, V2] = canMapSelf[Int, V2]
  implicit def canMapSelfFloat[V2]: CanMapValues[Float, Float, V2, V2] = canMapSelf[Float, V2]
  implicit def canMapSelfLong[V2]: CanMapValues[Long, Long, V2, V2] = canMapSelf[Long, V2]
  implicit def canMapSelfShort[V2]: CanMapValues[Short, Short, V2, V2] = canMapSelf[Short, V2]
  implicit def canMapSelfByte[V2]: CanMapValues[Byte, Byte, V2, V2] = canMapSelf[Byte, V2]
  implicit def canMapSelfChar[V2]: CanMapValues[Char, Char, V2, V2] = canMapSelf[Char, V2]

  //
  // Arrays
  //

  class OpArray[@spec(Double, Int, Float, Long) A, @spec(Double, Int, Float, Long) B: ClassTag]
    extends CanMapValues[Array[A], A, B, Array[B]] {

    /**Maps all values from the given collection. */
    def map(from: Array[A], fn: (A) => B): Array[B] = {
      val arr = new Array[B](from.length)
      cforRange(0 until from.length) { i =>
        arr(i) = fn(from(i))
      }
      arr
    }

    override def mapActive(from: Array[A], fn: A => B): Array[B] = map(from, fn)
  }

  implicit def opArray[@spec A, @spec B: ClassTag]: OpArray[A, B] =
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

sealed trait CanMapValuesLowPrio { self: CanMapValues.type =>

  def canMapSelf[V, V2]: CanMapValues[V, V, V2, V2] = {
    new CanMapValues[V, V, V2, V2] {
      def map(from: V, fn: (V) => V2): V2 = fn(from)
      def mapActive(from: V, fn: (V) => V2): V2 = fn(from)
    }
  }

}
