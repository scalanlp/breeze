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

/**
 * Marker for being able to zip two From's and map the values to a new collection
 *
 * @author dlwh
 */
trait CanZipMapValues[From, @specialized(Int, Float, Double) A, @specialized(Int, Float, Double) B, +To] {
  /** Maps all corresponding values from the two collection. */
  def map(from : From, from2: From, fn : (A,A)=>B) : To
}

object CanZipMapValues {
  type Op[From, A, B, To] = CanZipMapValues[From, A, B, To]

  //
  // Arrays
  //

  class OpArray[@specialized(Int, Float, Double) A, @specialized(Int, Float, Double) B: ClassManifest]
    extends Op[Array[A], A, B, Array[B]] {

    /**Maps all values from the given collection. */
    def map(from: Array[A], from2: Array[A], fn: (A, A) => B) = {
      require(from.length == from2.length, "Array lengths don't match!")
      val arr = new Array[B](from.length)
      for(i <- 0 until from.length) {
        arr(i) = fn(from(i), from2(i))
      }
      arr
    }

  }


  implicit def opArray[@specialized A, @specialized B: ClassManifest] =
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
