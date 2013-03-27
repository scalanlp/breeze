package breeze.stats.distributions

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
import breeze.linalg.DenseVector


@RunWith(classOf[JUnitRunner])
class DirichletTest extends FunSuite with Checkers {

  test("logDraw for small values") {
    val g = new Dirichlet(DenseVector(1E-5, 5.0, 50.0))
    assert(Array.fill(1000)(g.logDraw()).forall(_(0) > Double.NegativeInfinity))
  }


}  
