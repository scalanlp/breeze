package breeze.linalg
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
}
