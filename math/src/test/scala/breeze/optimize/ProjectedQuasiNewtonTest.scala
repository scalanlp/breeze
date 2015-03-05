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

import breeze.optimize.proximal.QuadraticMinimizer
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import breeze.linalg._
import breeze.numerics._

@RunWith(classOf[JUnitRunner])
class ProjectedQuasiNewtonTest extends PropSpec with PropertyChecks with OptimizeTestBaseTrait with VectorMatchers with Matchers {

  property("optimize a simple multivariate gaussian") {
    val optimizer = new ProjectedQuasiNewton(tolerance = 1.0E-9)
    forAll { init: DenseVector[Double] =>
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (sum((x - 3.0) :^ 2.0), (x * 2.0) - 6.0)
        }
      }

      val result = optimizer.minimize(f, init)
      result should beSimilarTo(DenseVector.fill(result.size)(3.0), allowedDeviation = 1E-5)
    }
  }

  property("optimize a simple multivariate gaussian with projection") {
    val optimizer = new ProjectedQuasiNewton(tolerance = 1.0E-5, projection = _.map(scala.math.min(_, 2.0)))

    forAll { init: DenseVector[Double] =>
      init := clip(init, Double.NegativeInfinity, 2.0)
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (sum((x - 3.0) :^ 4.0), (x - 3.0) :^ 3.0 :* 4.0)
        }
      }

      val result = optimizer.minimize(f, init)
      result should beSimilarTo(DenseVector.fill(result.size)(2.0), allowedDeviation = 1E-10)
    }
  }

  property("optimize a simple multivariate gaussian with l2 regularization") {
    val optimizer = new ProjectedQuasiNewton(tolerance = 1.0E-5)

    forAll { init: DenseVector[Double] =>
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (norm((x - 3.0) :^ 2.0, 1), (x * 2.0) - 6.0)
        }
      }

      val targetValue = 3 / (1.0 / 2 + 1)
      val result = optimizer.minimize(DiffFunction.withL2Regularization(f, 1.0), init)
      result should beSimilarTo(DenseVector.ones[Double](init.size) * targetValue, allowedDeviation = 3E-3 * result.size)
    }
  }

  property("optimize a complicated function without projection") {
    val optimizer = new ProjectedQuasiNewton(tolerance = 1.0E-5)

    forAll { a: DenseVector[Double] =>
      whenever(min(a) >= -3.0 && max(a) <= 3.0) {
        val init = DenseVector.rand(a.size)
        val f = new DiffFunction[DenseVector[Double]] {
          def calculate(x: DenseVector[Double]) = {
            (sum(exp((x :^ 2.0) :- (a :* x))), (x * 2.0 :- a) :* exp(x :^ 2.0 :- a :* x))
          }
        }

        val result = optimizer.minimize(f, init)
        val minimum = f(a / 2.0)
        f(result) should be(minimum +- abs(minimum) * 1E-2)
      }
    }
  }

  property("simple linear solve without projection") {
    val n = 5
    val H = new DenseMatrix(n, n, Array(1.8984250861699135,0.5955576666769438,-1.484430453342902,-1.0434994471390804,-3.675310432634351,
        0.5955576666769438,0.9090751938470876,-2.146380947361661,-0.13037609428980368,-0.40639564652095117,
        -1.484430453342902,-2.146380947361661,10.262733520770384,-6.097698907163584,2.29625304115155,
        -1.0434994471390804,-0.13037609428980368,-6.097698907163584,27.775920405610677,-5.574220233644466,
        -3.675310432634351,-0.40639564652095117,2.29625304115155,-5.574220233644466,12.21329172136971))
    val f = DenseVector(-1.2320199653150048, -0.14220655875869606, 0.38477404739124765, -0.3480575854151014, -0.4729810900829228)

    val cost = QuadraticMinimizer.Cost(H, f:*(-1.0))
    val init = DenseVector.zeros[Double](n)

    init := 0.0
    val x = H \ f

    init := 0.0
    val nlResult = new ProjectedQuasiNewton().minimize(cost, init)
    assert(norm(x - nlResult, inf) < 1e-4)
  }
}
