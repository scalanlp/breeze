/*
 *
 *  Copyright 2014 David Hall
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
import scala.{specialized => spec}
import scala.reflect.ClassTag

/**
 * Marker for being able to zip two collection objects (From[V]) and map the values to a new collection (To[Vout]).
 *
 * @author dlwh
 */
trait CanZipMapKeyValues[From, @spec(Int) K, @spec(Double, Int, Float, Long) V, @spec(Double, Int, Float, Long) RV, +To] {

  /** Maps all corresponding values from the two collections. */
  def map(from: From, from2: From, fn : (K,V,V)=>RV): To

}

object CanZipMapKeyValues {

  def canZipMapSelf[S]: CanZipMapKeyValues[S, Unit, S, S, S] = new CanZipMapKeyValues[S, Unit, S, S, S] {
    /** Maps all corresponding values from the two collections. */
    /** Maps all corresponding values from the two collections. */
    override def map(from: S, from2: S, fn: (Unit, S, S) => S): S = fn((), from, from2)
  }


  //
  // Arrays
  //

  class OpArray[@spec(Double, Int, Float, Long) V, @spec(Double, Int, Float, Long) RV: ClassTag]
    extends CanZipMapKeyValues[Array[V], Int, V, RV, Array[RV]] {

    /**Maps all values from the given collection. */
    def map(from: Array[V], from2: Array[V], fn: (Int, V, V) => RV) = {
      require(from.length == from2.length, "Array lengths don't match!")
      val arr = new Array[RV](from.length)
      for(i <- 0 until from.length) {
        arr(i) = fn(i, from(i), from2(i))
      }
      arr
    }

  }

  // <editor-fold defaultstate="collapsed" desc=" implicit CanZipMapValues[V, RV] implementations ">

  implicit def opArray[@spec V, @spec RV: ClassTag] = new OpArray[V, RV]

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

  // </editor-fold>

}
