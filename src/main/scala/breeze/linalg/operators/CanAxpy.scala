package breeze.linalg.operators

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
 * This is the capability trait for operations of the form y += x * a
 * @tparam A Scalar
 * @tparam X Source
 * @tparam Y Sink
 */
trait CanAxpy[A, X, Y] {
  def apply(a: A, x: X, y: Y)
}

object CanAxpy {
  implicit val canAxpyDoubleArray:CanAxpy[Double, Array[Double], Array[Double]] = {
    new CanAxpy[Double, Array[Double], Array[Double]] {
      def apply(a: Double, x: Array[Double], y: Array[Double]) {
        require(x.length == y.length, "Arrays must have the same length!")
        if (a == 1.0) {
          var i = 0
          while(i < x.length) {
            y(i) += x(i)
            i += 1
          }
        } else if (a != 0.0) {
          var i = 0
          while(i < x.length) {
            y(i) += x(i) * a
            i += 1
          }
        }
      }
    }
  }

}





