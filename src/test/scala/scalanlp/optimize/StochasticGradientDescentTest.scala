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
import scalanlp.counters.Counters._;

@RunWith(classOf[JUnitRunner])
class StochasticGradientDescentTest extends FunSuite with Checkers {
  import Arbitrary._;
  implicit val arbVector : Arbitrary[Vector] = Arbitrary(for {
    n <- arbitrary[Int] suchThat { _ > 0 } suchThat { _ < 4000};
    d <- arbitrary[Double]
  } yield ( (rand(n) * d value) : Vector));

  implicit val arbDoubleCounter: Arbitrary[DoubleCounter[String]] = Arbitrary(for {
    v <- arbitrary[Vector]
  } yield {
    val c = DoubleCounter[String]();
    for(i <- 0 until v.size) {
      c(i + "") = v(i);
    }
    c
  });



  test("optimize a simple multivariate gaussian") {
    val sgd = new StochasticGradientDescent[Int,Vector](1.,100,1);

    def optimizeThis(init: Vector) = {
      val f = new BatchDiffFunction[Int,Vector] {
        def valueAt(x: Vector, r: Seq[Int]) = {
          norm((x -3) :^ 2,1)
        }
        def gradientAt(x: Vector, batch:Seq[Int]):Vector = {
          (x * 2) - 6 value;
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init) 
      norm(result :- ones(init.size) * 3,2) < 1E-10
    }

    check(Prop.forAll(optimizeThis _));

  }

  test("optimize a simple multivariate gaussian with counters") {
    val sgd = new StochasticGradientDescent[String,DoubleCounter[String]](1.,100,1);

    def optimizeThis(init: DoubleCounter[String]) = {
      val f = new BatchDiffFunction[String,DoubleCounter[String]] {
        def valueAt(x: DoubleCounter[String], r: Seq[Int]) = {
          norm((x -3) :^ 2,1)
        }
        def gradientAt(x: DoubleCounter[String], batch: Seq[Int]):DoubleCounter[String] = {
          (x * 2) - 6 value;
        }
        val fullRange = 0 to 1;
      }

      val result = sgd.minimize(f,init);
      (!result.exists{ case(k,v) => Math.abs(v - 3.0) > 1E-3}
     && !result.exists(_._2.isNaN));
    }

    check(Prop.forAll(optimizeThis _ ));

  }
}
