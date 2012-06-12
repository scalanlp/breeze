package breeze.stats.distributions;

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

import breeze.util.I;

@RunWith(classOf[JUnitRunner])
class BernoulliTest extends FunSuite with Checkers with MomentsTestBase[Boolean] with ExpFamTest[Bernoulli,Boolean] {
  val expFam = Bernoulli

  import Arbitrary.arbitrary;

  def arbParameter = Arbitrary(arbitrary[Double].map(x => math.abs(x) % 1.0))

  def paramsClose(p: Double, b: Double) = if(b == 0.0) p < 1E-4 else (p -b).abs / b.abs.max(1E-4) < 1E-1

  implicit def arbDistr = Arbitrary {
    for(p <- arbitrary[Double].map{x => math.abs(x) % 1.0+1E-4}) yield new Bernoulli(p);
  }

  def asDouble(x: Boolean) = I(x)
  def fromDouble(x: Double) = x != 0.0


}
