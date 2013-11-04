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
import org.scalatest.matchers.ShouldMatchers
import scala.math._

import breeze.linalg._

@RunWith(classOf[JUnitRunner])
class ProjectedQuasiNewtonTest extends PropSpec with PropertyChecks with ShouldMatchers with OptimizeTestBaseTrait with VectorMatchers {

  property("optimize a simple multivariate gaussian") {
    val optimizer = new ProjectedQuasiNewton(tolerance = 1.0E-9)
    forAll { init: DenseVector[Double] =>
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (((x - 3.0) :^ 2.0).sum, (x * 2.0) - 6.0)
        }
      }

      val result = optimizer.minimize(f, init)
      result should beSimilarTo(DenseVector.fill(result.size)(3.0), allowedDeviation = 1E-5)
    }
  }

  property("optimize a simple multivariate gaussian with projection") {
    val optimizer = new ProjectedQuasiNewton(tolerance = 1.0E-5, projection = _.map(scala.math.min(_, 2.0)))

    forAll { init: DenseVector[Double] =>
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (((x - 3.0) :^ 4.0).sum, (x - 3.0) :^ 3.0 :* 4.0)
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
      whenever(a.min >= -3.0 && a.max <= 3.0) {
        val init = DenseVector.rand(a.size)
        val f = new DiffFunction[DenseVector[Double]] {
          def calculate(x: DenseVector[Double]) = {
            (breeze.numerics.exp((x :^ 2.0) :- (a :* x)).sum, (x * 2.0 :- a) :* breeze.numerics.exp(x :^ 2.0 :- a :* x))
          }
        }

        val result = optimizer.minimize(f, init)
        val minimum = f(a / 2.0)
        f(result) should be(minimum plusOrMinus abs(minimum) * 1E-2)
      }
    }
  }
}
