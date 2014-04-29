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
import scala.reflect.ClassTag
import breeze.linalg.support.CanZipAndTraverseValues.PairValuesVisitor

/**
 * Marker for being able to traverse over the values in a pair oof collections
 *
 * @author stucchio
 */
trait CanZipAndTraverseValues[From, From2,A,B] {
  /**Traverses all values from the given collection. */
  def traverse(from1: From, from2: From2, fn: PairValuesVisitor[A,B]): Unit
}

object CanZipAndTraverseValues {
  trait PairValuesVisitor[A,B] {
    def visit(a: A, b: B)
    def visitArray(arr: Array[A], arr2: Array[B]):Unit = {
      var i = 0
      if (arr.size != arr2.size) {
        throw new IllegalArgumentException("Arrays to be visited must have same size")
      }
      while(i < arr.size) {
        visit(arr(i), arr2(i))
        i += 1
      }
    }
  }
}
