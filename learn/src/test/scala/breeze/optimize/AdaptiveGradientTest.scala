package breeze.optimize

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

import scalala.tensor._;
import dense.DenseVector
import domain.IndexDomain
import scalala.library.Library._
import scalala.generic.collection.{CanViewAsTensor1, CanMapKeyValuePairs}
;

@RunWith(classOf[JUnitRunner])
class AdaptiveGradientTest extends OptimizeTestBase {

  test("optimize a simple multivariate gaussian, l2") {

    def optimizeThis(init2: DenseVector[Double], reg: Double) = {
      val init = init2 % 100;
      val sgd = new StochasticGradientDescent[DenseVector[Double]](20,200) with AdaptiveGradientDescent.L2Regularization[DenseVector[Double]] {
        override val lambda = reg.abs % 1E4;
      }
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init)
      val targetValue = 3 / (sgd.lambda / 2 + 1);
      val ok = norm(result :- DenseVector.ones[Double](init.size) * targetValue,2)/result.size < 2E-3
      if(!ok) {
        sys.error("min " + init + " with reg: " + sgd.lambda + "gives " + result + " should be " + targetValue);
      }
      ok
    }

    check(Prop.forAll( optimizeThis _))

  }

  test("optimize a simple multivariate gaussian, l1") {

    def optimizeThis(init2: DenseVector[Double], reg: Double) = {
      val init = init2 % 100;
      val sgd = new AdaptiveGradientDescent.L1Regularization[DenseVector[Double]](reg.abs%10, 1E-5, 50,200);
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init)
      val targetValue = if(sgd.lambda/2 > 3) 0.0 else  3 - sgd.lambda / 2;
      val ok = norm(result :- DenseVector.ones[Double](init.size) * targetValue,2)/result.size < 1E-3
      if(!ok) {
        sys.error("min " + init + " with reg: " + sgd.lambda + "gives " + result + " " + " should be " + targetValue);
      }
      ok
    }

    check(Prop.forAll( optimizeThis _))

  }


}