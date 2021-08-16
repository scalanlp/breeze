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
import breeze.linalg.norm
import breeze.linalg.operators.OpSub

/**
 * TODO
 *
 * @author dlwh
 **/
trait NormBasedDistance extends UFunc {

  implicit def distanceFromNormAndSub[T, U, V](
      implicit subImpl: OpSub.Impl2[T, U, V],
      normImpl: norm.Impl2[V, Double, Double]): Impl2[T, U, Double] = {

    new Impl2[T, U, Double] {
      def apply(v: T, v2: U): Double = {
        norm(subImpl(v, v2), normConstant)
      }
    }
  }

  protected def normConstant: Double

}
