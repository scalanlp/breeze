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

import org.scalatest.junit._
import org.scalacheck._
import org.junit.runner.RunWith

import breeze.linalg._

@RunWith(classOf[JUnitRunner])
class TruncatedNewtonMinimizerTest extends OptimizeTestBase {

  test("optimize a simple multivariate gaussian") {
    val lbfgs = new TruncatedNewtonMinimizer[DenseVector[Double], EmpiricalHessian[DenseVector[Double]]](100)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (((x - 3.0) :^ 2.0).sum, (x * 2.0) - 6.0)
        }
      }

      val empF = SecondOrderFunction.empirical(f)
      val result = lbfgs.minimize(empF, init)
      norm(result - 3.0, 2) < 1E-10
    }

    check(Prop.forAll(optimizeThis _))

  }

  test("optimize a simple multivariate gaussian with counters") {
    val lbfgs = new TruncatedNewtonMinimizer[Counter[String, Double], EmpiricalHessian[Counter[String, Double]]](100)

    def optimizeThis(init: Counter[String, Double]) = {
      val f = new DiffFunction[Counter[String, Double]] {
        def calculate(x: Counter[String, Double]) = {
          (((x - 3.0) dot (x - 3.0)), (x * 2.0) - 6.0)
        }
      }

      val empF = SecondOrderFunction.empirical(f)
      val result = lbfgs.minimize(empF, init)
      norm(result - 3.0, 2) < 1E-5
    }

    check(Prop.forAll(optimizeThis _))

  }



}
