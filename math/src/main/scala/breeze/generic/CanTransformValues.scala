package breeze.generic

import breeze.math.Complex

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

/**
 * Marker for being able to transform the keys and values in a value collection
 * to new values.
 *
 * @author dramage
 * @author dlwh
 */
trait CanTransformValues[-From, +A, -B] {
  /**Transforms all key-value pairs from the given collection. */
  def transform(from: From, fn: (A => B))

  /**Transforms all active key-value pairs from the given collection. */
  def transformActive(from: From, fn: (A => B))
}



object CanTransformValues {
  type Op[From, A, B] = CanTransformValues[From, A, B]

  //
  // Arrays
  //

  class OpArray[@specialized(Int, Float, Double) A, @specialized(Int, Float, Double) B<:A]
    extends Op[Array[A], A, B] {

    /**Transforms all values from the given collection. */
    def transform(from: Array[A], fn: (A) => B) {
      for(i <- 0 until from.length) {
        from(i) = fn(from(i))
      }
    }

    /**Transforms all active key-value pairs from the given collection. */
    def transformActive(from: Array[A], fn: (A) => B) { transform(from, fn)}
  }


  implicit def opArray[@specialized A, @specialized B <: A] =
    new OpArray[A, B]

  implicit object OpArrayII extends OpArray[Int, Int]

  implicit object OpArraySS extends OpArray[Short, Short]

  implicit object OpArrayLL extends OpArray[Long, Long]

  implicit object OpArrayFF extends OpArray[Float, Float]

  implicit object OpArrayDD extends OpArray[Double, Double]

  implicit object OpArrayCC extends OpArray[Complex, Complex]
}