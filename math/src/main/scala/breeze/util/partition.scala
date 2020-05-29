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

package breeze.util

import breeze.generic.UFunc
import breeze.linalg.support.CanCopy

/**
 *
 * Creates a copy of the array with its elements rearranged in such a way that the value of the element
 * in kth position is in the position it would be in a sorted array. All elements smaller than the kth element
 * are moved to the start of the array, all greater are moved to the end. Equal elements will be on the
 * immeditate left or right.
 * The ordering of the elements in the two partitions is undefined.
 *
 * Based on the numpy method of the same name. Docs lifted from numpy
 *
 * @author dlwh
 **/
object partition extends UFunc {

  implicit def inPlaceFromQSelectImplImpl[Arr, T](implicit
      qs: quickSelectImpl.Impl2[Arr, Int, T]
  ): InPlaceImpl2[Arr, Int] = {
    new InPlaceImpl2[Arr, Int] {
      override def apply(v: Arr, v2: Int): Unit = {
        qs(v, v2)
      }
    }
  }

  implicit def implFromInPlaceAndcopy[Arr](implicit
      qs: InPlaceImpl2[Arr, Int],
      copy: CanCopy[Arr]
  ): Impl2[Arr, Int, Arr] = {
    new Impl2[Arr, Int, Arr] {
      override def apply(v: Arr, v2: Int): Arr = {
        val c = copy(v)
        qs(c, v2)
        c
      }
    }
  }

}
