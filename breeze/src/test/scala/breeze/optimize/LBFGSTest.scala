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
class LBFGSTest extends OptimizeTestBase {

  test("optimize a simple multivariate gaussian") {
    val lbfgs = new LBFGS[DenseVector[Double]](100,4)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (norm((x - 3.0) :^ 2.0, 1), (x * 2.0) - 6.0)
        }
      }

      val result = lbfgs.minimize(f,init) 
      norm(result - 3.0,2) < 1E-10
    }

    check(Prop.forAll(optimizeThis _))

  }

  test("optimize a simple multivariate gaussian with counters") {
    val lbfgsString = new LBFGS[Counter[String,Double]](1000,4)

    def optimizeThis(init: Counter[String,Double]) = {
      val f = new DiffFunction[Counter[String,Double]] {
        def calculate(x: Counter[String,Double]) = {
          ((x - 3.0) dot (x - 3.0),(x * 2.0) - 6.0)
        }
      }

      val result = lbfgsString.minimize(f,init)
      norm(result - 3.0,2) < 1E-5
    }

    check(Prop.forAll(optimizeThis _ ))

  }


  test("optimize a simple multivariate gaussian with l2 regularization") {
    val lbfgs = new LBFGS[DenseVector[Double]](10000,4)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (norm((x -3.0) :^ 2.0,1), (x * 2.0) - 6.0)
        }
      }

      val targetValue = 3 / (1.0 / 2 + 1)
      val result = lbfgs.minimize(DiffFunction.withL2Regularization(f, 1.0),init)
      val ok = norm(result :- DenseVector.ones[Double](init.size) * targetValue,2)/result.size < 3E-3
      ok || (throw new RuntimeException("Failed to find optimum for init " + init))
    }

    check(Prop.forAll(optimizeThis _))

   }



   test("lbfgs-c rosenbroch example") {
    val lbfgs = new LBFGS[DenseVector[Double]](40,6)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new DiffFunction[DenseVector[Double]] {

        def calculate(x: DenseVector[Double]) = {
          var fx = 0.0
          val g = DenseVector.zeros[Double](x.length)

          for(i <- 0 until x.length by 2) {
            val t1 = 1.0 - x(i)
            val t2 = 10.0 * (x(i+1) - x(i) * x(i))
            g(i+1) = 20 * t2
            g(i) = -2 * (x(i) * g(i+1) + t1)
            fx += t1 * t1 + t2 * t2

          }
          fx -> g

        }
      }

      val targetValue = 1.0

      val result = lbfgs.minimize(f,init)

      val ok = norm(result :- DenseVector.ones[Double](init.size) * targetValue,2)/result.size < 1E-5
      ok || (throw new RuntimeException("Failed to find optimum for init " + init))
    }
    val init = DenseVector.zeros[Double](100)
    init(0 until init.length by 2) := -1.2
    init(1 until init.length by 2) := 1.0
    assert(optimizeThis(init))


   }




}

