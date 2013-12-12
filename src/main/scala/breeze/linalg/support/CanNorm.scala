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
import breeze.macros.expand


/**
 * Construction delegate for getting the norm of a value of type From.
 *
 * @author dramage, dlwh
 */
trait CanNorm[-From, NormType] {
  def apply(v1: From, v2: NormType): Double
}

object CanNorm {
  @expand
  @expand.valify
  implicit def scalarNorm[@expand.args(Int, Long, Float, Double) T]: CanNorm[T, Unit] = new CanNorm[T, Unit] {
    def apply(v1: T, v2: Unit): Double = v1.abs.toDouble
  }

  implicit def standardNormFromDoubleNorm[T](implicit norm: CanNorm[T, Double]):CanNorm[T, Unit] = new CanNorm[T, Unit] {
    def apply(v1: T, v2: Unit): Double = norm(v1, 2.0)
  }

  implicit def intNormFromDoubleNorm[T](implicit norm: CanNorm[T, Double]):CanNorm[T, Int] = new CanNorm[T, Int] {
    def apply(v1: T, v2: Int): Double = norm(v1, v2.toDouble)
  }
}
