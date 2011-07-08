package scalanlp.optimize

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

import scalala.tensor.dense.DenseVector
import scalala.tensor.mutable.Counter
import scalala.library.Library.norm;

@RunWith(classOf[JUnitRunner])
class FobosTest extends OptimizeTestBase {


  test("optimize a simple multivariate gaussian, l2") {

    def optimizeThis(init: DenseVector[Double], reg: Double) = {
      val sgd = new StochasticGradientDescent.SimpleSGD[DenseVector[Double]](2.,100) with Fobos.L2Regularization[DenseVector[Double]] {
        override val lambda = reg.abs;
      }
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init)
      val targetValue = 3 / (reg.abs / 2 + 1);
      val ok = norm(result :- DenseVector.ones[Double](init.size) * targetValue,2) < 1E-10
      if(!ok) {
        error("min " + init + " with reg: " + reg + "gives " + result);
      }
      ok
    }

    check(Prop.forAll( optimizeThis _))

  }

  test("optimize a simple multivariate gaussian, l1") {

    def optimizeThis(init: DenseVector[Double], reg: Double) = {
      val sgd = new StochasticGradientDescent.SimpleSGD[DenseVector[Double]](2.,100) with Fobos.L1Regularization[Int,DenseVector[Double]] {
        override val lambda = reg.abs;
      }
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init)
      val targetValue = if(sgd.lambda/2 > 3) 0.0 else  3 - sgd.lambda / 2;
      val ok = norm(result :- DenseVector.ones[Double](init.size) * targetValue,2) < 1E-10
      if(!ok) {
        error("min " + init + " with reg: " + reg + "gives " + result);
      }
      ok
    }

    check(Prop.forAll( optimizeThis _))

  }


}