package scalanlp.stats.sampling;

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

import scalanlp.stats.DescriptiveStats._

@RunWith(classOf[JUnitRunner])
class GaussianTest extends FunSuite with Checkers with MomentsTestBase[Double] {


  import Arbitrary.arbitrary;
  test("Probability of mean") {
    check( Prop.forAll { (m: Double, s: Double)=> (s == 0) || {
        val b = new Gaussian(mu=m,sigma=s.abs);
        b.unnormalizedLogPdf(m) == 0.0;
      }
    })
  }

  implicit def arbDistr = Arbitrary {
    for(mean <- arbitrary[Double].map{_.abs % 10000.0};
        std <- arbitrary[Double].map {_.abs % 8.0 + .1}) yield new Gaussian(mean,std);
  }

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x
}
