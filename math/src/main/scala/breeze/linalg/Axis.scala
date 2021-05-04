package breeze.linalg

import breeze.generic.UFunc.{UImpl, UImpl2, UImpl3}
import breeze.linalg.support.CanCollapseAxis

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
 * This trait is commonly used for [[breeze.linalg.sum]] and its kin for summing along
 * a particular axis of a Matrix.
 * @author dlwh
 */
sealed trait Axis
object Axis {
  type Value = Axis
  case object _0 extends Axis
  case object _1 extends Axis

  implicit def collapseUred[Tag, V1, AxisT <: Axis, TA, VR, Result](
                                                                     implicit handhold: CanCollapseAxis.HandHold[V1, AxisT, TA],
                                                                     impl: UImpl[Tag, TA, VR],
                                                                     collapse: CanCollapseAxis[V1, AxisT, TA, VR, Result]): UImpl2[Tag, V1, AxisT, Result] = {
    new UImpl2[Tag, V1, AxisT, Result] {
      def apply(v: V1, v2: AxisT): Result = collapse.apply(v, v2)(impl(_))
    }
  }

  implicit def collapseUred3[Tag, V1, AxisT <: Axis, V3, TA, VR, Result](
                                                                          implicit handhold: CanCollapseAxis.HandHold[V1, AxisT, TA],
                                                                          impl: UImpl2[Tag, TA, V3, VR],
                                                                          collapse: CanCollapseAxis[V1, AxisT, TA, VR, Result]): UImpl3[Tag, V1, AxisT, V3, Result] = {
    new UImpl3[Tag, V1, AxisT, V3, Result] {
      def apply(v: V1, v2: AxisT, v3: V3): Result = collapse.apply(v, v2)(impl(_, v3))
    }
  }
}
