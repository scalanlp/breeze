package breeze.linalg
package support
/*
 Copyright 2012 Daniel Ramage

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

import breeze.math.Ring


/**
 * Construction delegate for getting the norm of a value of type From.
 *
 * @author dramage
 */
trait CanNorm[-From] {
  def apply(v1: From, v2: Double): Double
}

object CanNorm {
  implicit def mkTensor1Norm[T, V](implicit tt : T=>Vector[V], ring: Ring[V]): CanNorm[T] = new CanNorm[T] {
    def apply(t : T, n : Double) : Double =
      tt(t).norm(n)
  }
}
