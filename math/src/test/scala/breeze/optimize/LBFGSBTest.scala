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
import breeze.linalg.{DenseVector, norm}
import breeze.numerics.pow
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by fanming.chen on 2015/3/14 0014.
 */
@RunWith(classOf[JUnitRunner])
class LBFGSBTest extends OptimizeTestBase {
  val EPS = 1E-4;

  test("L-BFGS-B should solve with bound constraint") {
    val solver = new LBFGSB(DenseVector[Double](-100, -100), DenseVector[Double](1200, 100))
    val nearX0 = DenseVector[Double](-1.2, 1.0)

    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val cost = (1 - x(0)) * (1 - x(0)) + 100 * pow(x(1) - x(0) * x(0), 2)
        val grad = DenseVector(
          -2.0 * (1 - x(0)) + 200 * (x(1) - x(0) * x(0)) * (-2.0 * x(0)),
          200 * (x(1) - x(0) * x(0))
        )
        (cost, grad)
      }
    }

    var optX = solver.minimize(f, nearX0)

    val expectFx = 0.0
    assert(norm(f(optX)) < EPS)

    val farX0 = DenseVector[Double](1200.0, 8.2)
    assert(norm(f(optX)) < EPS)
  }

  import breeze.numerics._
  test("L-BFGS-B should solve with unbound constraint") {
    val solver = new LBFGSB(DenseVector[Double](-inf, -inf), DenseVector(inf, inf))
    val nearX0 = DenseVector[Double](-1.2, 1.0)

    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val cost = pow(x(0) - x(1) * x(1), 2) / 2.0 + (x(1) - 2.0) * (x(1) - 2.0) / 2.0
        val grad = DenseVector(
          x(0) - x(1) * x(1),
          -2.0 * x(1) * (x(0) - x(1) * x(1)) + x(1) - 2.0
        )
        (cost, grad)
      }
    }

    var optX = solver.minimize(f, nearX0)
    val expectFx = 0.0
    assert(norm(f(optX)) < EPS)

    val farX0 = DenseVector[Double](12.0, 8.2)
    optX = solver.minimize(f, farX0)
    assert(norm(f(optX)) < EPS)
  }

  test("alglib example") {
    //    http://www.alglib.net/translator/man/manual.cpython.html#example_minbleic_d_1
    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val func = 100 * math.pow(x(0) + 3, 4) + math.pow(x(1) - 3, 4)
        val grad = DenseVector(
          400 * math.pow(x(0) + 3, 3),
          4 * math.pow(x(1) - 3, 3)
        )
        func -> grad
      }
    }

    val usolver = new LBFGSB(DenseVector[Double](-inf, -inf), DenseVector(inf, inf))
    val solver = new LBFGSB(DenseVector[Double](-1, -1), DenseVector(1, 1))
    val res = solver.minimizeAndReturnState(f, DenseVector.zeros[Double](2))
    assert(res.x == DenseVector(-1.0, 1.0))
//    assert(res.value == 4.0)
    val ures = usolver.minimizeAndReturnState(f, DenseVector.zeros[Double](2))
    assert(ures.value < res.value)
  }

  test("issue 572") {
    val solver = new LBFGSB(DenseVector[Double](1E-12), DenseVector[Double](Double.MaxValue))

    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val cost = x(0) + 1.0 / x(0)
        val grad = DenseVector(1.0 - 1.0 / (x(0) * x(0)))
        (cost, grad)
      }
    }

    val nearX0 = DenseVector[Double](1.5)
    val nearRes = solver.minimizeAndReturnState(f, nearX0)
    assert(abs(nearRes.x(0) - 1.0) < EPS)

    val farX0 = DenseVector[Double](1500)
    val farRes = solver.minimizeAndReturnState(f, farX0)
    assert(abs(farRes.x(0) - 1.0) < EPS)
  }

  test("issue #497") {
    val solver = new LBFGSB(DenseVector[Double](-100, -100), DenseVector[Double](0.5, 100))
    val nearX0 = DenseVector[Double](-1.2, 1.0)

    val f = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val cost = (1 - x(0)) * (1 - x(0)) + 100 * pow(x(1) - x(0) * x(0), 2)
        val grad = DenseVector(
          -2.0 * (1 - x(0)) + 200 * (x(1) - x(0) * x(0)) * (-2.0 * x(0)),
          200 * (x(1) - x(0) * x(0))
        )
        (cost, grad)
      }
    }

    val optX = solver.minimize(f, nearX0)
    assert(optX == DenseVector(0.5, 0.25))
  }
}
