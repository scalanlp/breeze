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
class StochasticGradientDescentTest extends FunSuite with Checkers {
  import Arbitrary._;
  implicit val arbVector : Arbitrary[DenseVector[Double]] = Arbitrary(for {
    n <- arbitrary[Int] suchThat { _ > 0 } suchThat { _ < 4000};
    d <- arbitrary[Double]
  } yield ( (DenseVector.rand(n) * d) : DenseVector[Double]));

  implicit val arbDoubleCounter: Arbitrary[Counter[String,Double]] = Arbitrary(for {
    v <- arbitrary[DenseVector[Double]]
  } yield {
    val c = Counter[String,Double]();
    for(i <- 0 until v.size) {
      c(i + "") = v(i);
    }
    c
  });



  test("optimize a simple multivariate gaussian") {
    val sgd = StochasticGradientDescent[DenseVector[Double]](2.,100,1);

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init) 
      norm(result :- DenseVector.ones[Double](init.size) * 3,2) < 1E-10
    }

    check(Prop.forAll(optimizeThis _));

  }

  test("optimize a simple multivariate gaussian with counters") {
    val sgd =  StochasticGradientDescent[Counter[String,Double]](1.,100,1);

    def optimizeThis(init: Counter[String,Double]) = {
      val f = new BatchDiffFunction[Counter[String,Double]] {
        def calculate(x: Counter[String,Double], r: IndexedSeq[Int]) = {
          (norm((x -3) :^ 2,1) , (x * 2) - 6);
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init);
      norm(result - 3.0,2) < 1E-3;
    }

    check(Prop.forAll(optimizeThis _ ));

  }
}
