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

import scalanlp.stats.DescriptiveStats._;

@RunWith(classOf[JUnitRunner])
class GammaTest extends FunSuite with Checkers {
  import Arbitrary.arbitrary;
  val arbDouble = Arbitrary(Arbitrary.arbitrary[Double].map(_ % 10000))
  test("mode") {
    implicit val ad = arbDouble;
    check( Prop.forAll { (k1: Double, t: Double, k2: Double)=>  t == 0 || k1.abs < 1 || k1 == k2 || {
        val b = new Gamma(k1.abs + 1,t.abs);
        // mode is floor( (n+1) * p)
        b.pdf( (k1.abs) * (t.abs) ) >= b.pdf( k2.abs)
      }
    })
  }

  val NUM_SAMPLES = 30000;
  val TOL = 1E-2;

  test("mean and variance -- sampling") {
    val n = 10;
    val d = 0.5;
    val b = new Gamma(n,d);
    val (m,v) = meanAndVariance(b.samples.take(NUM_SAMPLES).map(_.toDouble));
    (m - b.mean).abs < TOL && (v - b.variance).abs < TOL;
  }

}  
