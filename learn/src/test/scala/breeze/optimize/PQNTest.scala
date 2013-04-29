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

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith

import breeze.linalg._

@RunWith(classOf[JUnitRunner])
class PQNTest extends OptimizeTestBase {

  test("optimize a simple multivariate gaussian") {
    val optimizer = new PQN(optTol = 1.0E-5)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new ProjectableProblem {
        def calculate(x: DenseVector[Double]) = {
          (((x - 3.0) :^ 2.0).sum, (x * 2.0) - 6.0)
        }
      }

      val result = optimizer.minimize(f, init)
      norm(result - 3.0, 2) < 1E-10
    }

    check(Prop.forAll(optimizeThis _))
  }
  test("optimize a simple multivariate gaussian with projection") {
    val optimizer = new PQN(optTol = 1.0E-5)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new ProjectableProblem {
        def calculate(x: DenseVector[Double]) = {
          (((x - 3.0) :^ 2.0).sum, (x * 2.0) - 6.0)
        }
        override def project(x: DenseVector[Double]) = {
          x.map(scala.math.min(_, 2.0))
        }
      }

      val result = optimizer.minimize(f, init)
      norm(result - 2.0, 2) < 1E-10
    }

    check(Prop.forAll(optimizeThis _))
  }
  test("optimize a simple multivariate gaussian with l2 regularization") {
    val optimizer = new PQN(optTol = 1.0E-5)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new ProjectableProblem {
        def calculate(x: DenseVector[Double]) = {
          (norm((x - 3.0) :^ 2.0, 1), (x * 2.0) - 6.0)
        }
      }

      val targetValue = 3 / (1.0 / 2 + 1)
      val result = optimizer.minimize(ProjectableProblem.withL2Regularization(f, 1.0), init)
      val ok = norm(result :- DenseVector.ones[Double](init.size) * targetValue, 2) / result.size < 3E-3
      ok || (throw new RuntimeException("Failed to find optimum for init " + init))
    }

    check(Prop.forAll(optimizeThis _))

  }
}

