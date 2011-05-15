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
class BinomialTest extends FunSuite with Checkers {
  import Arbitrary.arbitrary;
  test("mode") {
    check( Prop.forAll { (n: Int, d2: Double, n2: Int)=>  n == Int.MaxValue || n <=  0 || n2 <= 0 || n.abs <= n2.abs || d2 == 0  || {
      val d = (d2 % 1.0) abs;
      if(d == 0.0) true
      else {
        val posn = n.abs % 1000;
        val posn2 = n2.abs % 1000;
        val b = new Binomial(posn,d);
        // mode is floor( (n+1) * p)
        val a = b.probabilityOf( d * (posn + 1) toInt )
        val bb = b.probabilityOf( d * posn2 toInt)
        println(d + " " + posn + " " + posn2 + " " + a + " " + bb)
        b.probabilityOf( d * (posn + 1) toInt ) >= b.probabilityOf( d * posn2 toInt)
      }
    }
    })
  }

  val NUM_SAMPLES = 30000;
  val TOL = 1E-2;

  test("mean and variance -- sampling") {
    val n = 10;
    val d = 0.5;
    val b = new Binomial(n,d);
    val (m,v) = meanAndVariance(b.samples.take(NUM_SAMPLES).map(_.toDouble));
    (m - b.mean).abs < TOL && (v - b.variance).abs < TOL;
  }

}  
