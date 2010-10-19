package scalanlp.optimize

/*
 Copyright 2010 David Hall, Daniel Ramage

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

import org.scalatest._
import scalala.tensor.dense.{DenseVector}

import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

import scalala.library.Vectors._;
import scalala.library.Random._;
import scalala.tensor.Vector;
import scalala.tensor.counters.Counters._;

import scalanlp.util._;

@RunWith(classOf[JUnitRunner])
class OWLQNTest extends FunSuite with Checkers {
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

  test("super simple") {
    val lbfgs = new OWLQN[Int,Vector](100,4);

    def optimizeThis(init: Vector) = {
      val f = new DiffFunction[Int,Vector] {
        def calculate(x: Vector) = {
          (norm((x -3) :^ 2,1),(x * 2) - 6 value);
        }
      }

      val result = lbfgs.minimize(f,init)
      result
    }

    val result = optimizeThis(DenseVector(3)(-1.1053));
    assert((result(0) - 2.5) < 1E-4, result)
  }


  test("optimize a simple multivariate gaussian") {
    val lbfgs = new OWLQN[Int,Vector](100,4,1.0);

    def optimizeThis(init: Vector) = {
      val f = new DiffFunction[Int,Vector] {
        def calculate(x: Vector) = {
          (norm((x -3) :^ 2,1),(x * 2) - 6 value);
        }
      }

      val result = lbfgs.minimize(f,init)
      val closeish = norm(result :- ones(init.size) * 2.5,2) < 1E-10
      if(closeish) {
        true
      } else {
        throw new Exception(result.toString + " is not close enough to 2.5");
      }
    }

    check(Prop.forAll(optimizeThis _));

  }

  test("optimize a simple multivariate gaussian with counters") {
    val lbfgsString = new OWLQN[String,DoubleCounter[String]](100,4, 1.0);

    def optimizeThis(init: DoubleCounter[String]) = {
      val f = new DiffFunction[String,DoubleCounter[String]] {
        def calculate(x: DoubleCounter[String]) = {
          (norm((x -3) :^ 2,1), (x * 2) - 6 value);
        }
      }

      val result = lbfgsString.minimize(f,init);
      if(!result.exists{ case(k,v) => math.abs(v - 2.5) > 1E-3} && !result.exists(_._2.isNaN)) {
        true
      } else {
        throw new Exception(result.toString + " is not close enough to 2.5");
      }
    }

    check(Prop.forAll(optimizeThis _ ));

  }
}
