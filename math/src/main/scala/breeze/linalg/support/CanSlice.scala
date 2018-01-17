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

/**
 * Capability trait for slices like denseVector(0 until 5)
 * @author dlwh
 */
trait CanSlice[-From, -Slice, +To] {
  def apply(from: From, slice: Slice): To
}

/**
 * Capability trait for slices like denseMatrix(1 until 5, 3 until 20 by 2)
 * @author dlwh
 */
trait CanSlice2[-From, -Slice1, -Slice2, +To] {
  def apply(from: From, slice: Slice1, slice2: Slice2): To
}
