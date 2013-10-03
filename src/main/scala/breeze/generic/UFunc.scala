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


/**
 * "Universal" Functions that mimic numpy's. A universal function is defined
 * on anything that supports elementwise maps
 *
 * For example, exp is a UFunc: it just calls exp on all components of the passed in
 * object.
 * @author dlwh
 */
trait UFunc[@specialized(Int, Float, Double) -V, @specialized(Int, Float, Double) +V2] {
  def apply(v: V):V2
  def apply[T,U](t: T)(implicit cmv: CanMapValues[T, V, V2, U]):U = cmv.map(t, apply _)
  def applyActive[T,U](t: T)(implicit cmv: CanMapValues[T, V, V2, U]):U = cmv.mapActive(t, apply _)

  def inPlace[T](t: T)(implicit ctv: CanTransformValues[T, V, V2]) { ctv.transform(t, apply _)}
}

object UFunc {
  /**
   * Creates a UFunc from the given Function1
   * @param f
   * @tparam V
   * @tparam V2
   * @return
   */
  def apply[@specialized(Int, Float, Double) V, @specialized(Int, Float, Double) V2](f: V=>V2):UFunc[V, V2] = new UFunc[V, V2] {
    def apply(v: V) = f(v)
  }
}
