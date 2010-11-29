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

import scalala.Scalala._;
import scalala.tensor.Vector;
import scalala.tensor.counters.Counters._;

@RunWith(classOf[JUnitRunner])
class AdaptiveGradientTest extends FunSuite with Checkers {
  import Arbitrary._;
  implicit val arbVector : Arbitrary[Vector] = Arbitrary(for {
    n <- arbitrary[Int] suchThat { _ > 0 }
    d <- arbitrary[Double]
  } yield ( (rand(n % 200 + 1) * d value) : Vector));

  implicit val arbDoubleCounter: Arbitrary[DoubleCounter[String]] = Arbitrary(for {
    v <- arbitrary[Vector]
  } yield {
    val c = DoubleCounter[String]();
    for(i <- 0 until v.size) {
      c(i + "") = v(i);
    }
    c
  });



  test("optimize a simple multivariate gaussian, l2") {

    def optimizeThis(init: Vector, reg: Double) = {
      val sgd = new StochasticGradientDescent[Int,Vector](20,200,1) with AdaptiveGradientDescent.L2Regularization[Int,Vector] {
        override val lambda = reg.abs;
      }
      val f = new BatchDiffFunction[Int,Vector] {
        def calculate(x: Vector, r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6 value);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init)
      val targetValue = 3 / (reg.abs / 2 + 1);
      val ok = norm(result :- ones(init.size) * targetValue,2)/result.size < 1E-3
      if(!ok) {
        error("min " + init + " with reg: " + reg + "gives " + result + " should be " + targetValue);
      }
      ok
    }

    check(Prop.forAll( optimizeThis _))

  }

  test("optimize a simple multivariate gaussian, l1") {

    def optimizeThis(init: Vector, reg: Double) = {
      val sgd = new StochasticGradientDescent[Int,Vector](50,200,1) with AdaptiveGradientDescent.L1Regularization[Int,Vector] {
        override val lambda = reg.abs % 10;
      }
      val f = new BatchDiffFunction[Int,Vector] {
        def calculate(x: Vector, r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6 value);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init)
      val targetValue = if(sgd.lambda/2 > 3) 0.0 else  3 - sgd.lambda / 2;
      val ok = norm(result :- ones(init.size) * targetValue,2)/result.size < 1E-3
      if(!ok) {
        error("min " + init + " with reg: " + reg + "gives " + result + " " + " should be " + targetValue);
      }
      ok
    }

    check(Prop.forAll( optimizeThis _))

  }


}