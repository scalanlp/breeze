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
import org.scalacheck._
import org.specs._;
import org.specs.matcher._;

import scalala.library.Vectors._;
import scalala.library.Random._;
import scalala.tensor.Vector;

object LBFGSSpecification extends Specification("LBFGS")  with ScalaCheckMatchers {
  import Arbitrary._;
  val arbVector = for {
    n <- arbitrary[Int] suchThat { _ > 0 };
    d <- arbitrary[Double]
  } yield ( (rand(n) * d value) : Vector);

  val lbfgs = new LBFGS(1E-4,100,4);
  "optimize a simple multivariate gaussian" in {

    def optimizeThis(init: Vector) = {
      val f = new DiffFunction {
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
}

import org.specs.runner._;
class LBFGSTest extends JUnit4(LBFGSSpecification);
