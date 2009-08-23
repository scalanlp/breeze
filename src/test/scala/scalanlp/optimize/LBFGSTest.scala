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

/*
import org.scalacheck._
import org.specs._;
import org.specs.matcher._;

import scalala.Scalala._;
import scalala.tensor.Vector;
import scalanlp.counters.Counters._;

object LBFGSSpecification extends Specification("LBFGS")  with ScalaCheckMatchers {
  import Arbitrary._;
  val arbVector = for {
    n <- arbitrary[Int] suchThat { _ > 0 };
    d <- arbitrary[Double]
  } yield ( (rand(n) * d value) : Vector);

  val arbDoubleCounter = for {
    v <- arbVector
  } yield {
    val c = DoubleCounter[String]();
    for(i <- 0 until v.size) {
      c(i + "") = v(i);
    }
    c
  }


  val lbfgs = new LBFGS[Int,Vector](1E-4,100,4);

  "optimize a simple multivariate gaussian" in {

    def optimizeThis(init: Vector) = {
      val f = new DiffFunction[Int,Vector] {
        def valueAt(x: Vector) = {
          norm((x -3) :^ 2,1)
        }
        def gradientAt(x: Vector):Vector = {
          (x * 2) - 6 value;
        }
      }

      val result = lbfgs.minimize(f,init) 
      norm(result - ones(init.size) * 3,2) < 1E-10
    }

    arbVector must pass { optimizeThis _ }

  }

  val lbfgsString = new LBFGS[String,DoubleCounter[String]](1E-4,100,4);
  "optimize a simple multivariate gaussian with counters" in {

    def optimizeThis(init: DoubleCounter[String]) = {
      val f = new DiffFunction[String,DoubleCounter[String]] {
        def valueAt(x: DoubleCounter[String]) = {
          norm((x -3) :^ 2,1)
        }
        def gradientAt(x: DoubleCounter[String]):DoubleCounter[String] = {
          (x * 2) - 6 value;
        }
      }

      val result = lbfgsString.minimize(f,init);
      println(result);
      (!result.exists{ case(k,v) => Math.abs(v - 3.0) > 1E-10}
     && !result.exists(_._2.isNaN));
    }

    arbDoubleCounter must pass { optimizeThis _ }

  }
}

import org.specs.runner._;
class LBFGSTest extends JUnit4(LBFGSSpecification);
*/
