package breeze.optimize
/*
 Copyright 2009 David Hall, Daniel Ramage

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

import org.scalacheck._
import breeze.linalg._

class StochasticGradientDescentTest extends OptimizeTestBase {

  test("optimize a simple multivariate gaussian") {
    val sgd = StochasticGradientDescent[DenseVector[Double]](2.0, 100)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          val r = x - 3.0
          (r.dot(r), (x * 2.0) - 6.0)
        }
        val fullRange = 0 to 1
      }

      val result = sgd.minimize(f, init)
      norm(result -:- DenseVector.ones[Double](init.size) * 3.0, 2) < 1e-10
    }

    check(Prop.forAll(optimizeThis _))

  }

  test("optimize a simple multivariate gaussian with counters") {
    val sgd = StochasticGradientDescent[Counter[String, Double]](1.0, 100)

    def optimizeThis(init: Counter[String, Double]) = {
      val f = new BatchDiffFunction[Counter[String, Double]] {
        def calculate(x: Counter[String, Double], r: IndexedSeq[Int]) = {
          val r = x - 3.0
          (r.dot(r), (x * 2.0) - 6.0)
        }
        val fullRange = 0 to 1
      }

      val result = sgd.minimize(f, init)
      norm(result - 3.0, 2) < 1e-3
    }

    check(Prop.forAll(optimizeThis _))

  }
}
