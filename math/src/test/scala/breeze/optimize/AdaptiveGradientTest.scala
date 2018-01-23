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
class AdaptiveGradientTest extends OptimizeTestBase {

  test("optimize a simple multivariate gaussian, l2") {

    def optimizeThis(init2: DenseVector[Double], reg: Double) = {
      val init = init2 % 100.0
      val sgd = new AdaptiveGradientDescent.L2Regularization[DenseVector[Double]](reg % 1E3 abs, 1, 1000)
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          (sum((x - 3.0) ^:^ 2.0), (x * 2.0) - 6.0)
        }
        val fullRange = 0 to 1
      }

      val result = sgd.minimize(f, init)
      val targetValue = 3 / (sgd.regularizationConstant / 2 + 1)
      val ok = norm(result -:- DenseVector.ones[Double](init.size) * targetValue, 2) / result.size < 2E-3
      if (!ok) {
        sys.error(
          "min " + init + " with reg: " + sgd.regularizationConstant + "gives " + result + " should be " + targetValue)
      }
      ok
    }

    check(Prop.forAll(optimizeThis _))

  }

  test("optimize a simple multivariate gaussian, l1") {

    def optimizeThis(init2: DenseVector[Double], reg: Double) = {
      val init = init2 % 100.0
      val sgd = new AdaptiveGradientDescent.L1Regularization[DenseVector[Double]](reg.abs % 10, 1E-7, 1, 600)
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          (sum((x - 3.0) ^:^ 2.0), (x * 2.0) - 6.0)
        }
        val fullRange = 0 to 1
      }

      val result = sgd.minimize(f, init)
      val targetValue = if (sgd.lambda / 2 > 3) 0.0 else 3 - sgd.lambda / 2
      val ok = norm(result -:- DenseVector.ones[Double](init.size) * targetValue, 2) / result.size < 1E-2
      if (!ok) {
        sys.error(s"min $init with reg: ${sgd.lambda} gives $result should be $targetValue")
      }
      ok
    }

    check(Prop.forAll(optimizeThis _))

  }

}
