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

package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.support.CanMapValues
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
object mapValues extends UFunc with mapValuesLowPrio {

  implicit def canMapSelfDouble[V2]: Impl2[Double, Double => V2, V2] = canMapSelf[Double, V2]
  implicit def canMapSelfInt[V2]: Impl2[Int, Int => V2, V2] = canMapSelf[Int, V2]
  implicit def canMapSelfFloat[V2]: Impl2[Float, Float => V2, V2] = canMapSelf[Float, V2]
  implicit def canMapSelfLong[V2]: Impl2[Long, Long => V2, V2] = canMapSelf[Long, V2]
  implicit def canMapSelfShort[V2]: Impl2[Short, Short => V2, V2] = canMapSelf[Short, V2]
  implicit def canMapSelfByte[V2]: Impl2[Byte, Byte => V2, V2] = canMapSelf[Byte, V2]
  implicit def canMapSelfChar[V2]: Impl2[Char, Char => V2, V2] = canMapSelf[Char, V2]

  //
  // Arrays
  //

  class OpArray[@spec(Double, Int, Float, Long) A, @spec(Double, Int, Float, Long) B: ClassTag]
      extends Impl2[Array[A], A => B, Array[B]] {

    /**Maps all values from the given collection. */
    def apply(from: Array[A], fn: (A) => B): Array[B] = {
      val arr = new Array[B](from.length)
      cforRange(0 until from.length) { i =>
        arr(i) = fn(from(i))
      }
      arr
    }
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

sealed trait mapValuesLowPrio {  self: mapValues.type =>

  /*implicit*/
  def canMapSelf[V, V2]: Impl2[V, V => V2, V2] = {
    new Impl2[V, V => V2, V2] {
      def apply(from: V, fn: (V) => V2) = fn(from)
      def mapActive(from: V, fn: (V) => V2) = fn(from)
    }
  }

}

object mapActiveValues extends UFunc {

  implicit def implFromCanMapValues[T, V, V2, R](implicit cmv: CanMapValues[T, V, V2, R]): Impl2[T, V => V2, R] =
    new Impl2[T, V => V2, R] {
      override def apply(v: T, v2: (V) => V2): R = cmv(v, v2)
    }

}
