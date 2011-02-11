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
class PoissonTest extends FunSuite with Checkers {
  import Arbitrary.arbitrary;

  val NUM_SAMPLES = 30000;
  val TOL = 1E-1;

  test("mean and variance -- sampling") {
    val mean = 5.0;
    val b = new Poisson(mean);
    val (m,v) = meanAndVariance(b.samples.take(NUM_SAMPLES).map(_.toDouble));
    assert( (m - b.mean).abs/mean < TOL, m + " vs " + b.mean)
    assert((v - b.variance).abs/mean < TOL, v + " vs " +  b.variance);
  }

  test("cdf") {
    val mean = 5.0
    val poi = new Poisson(mean);
    assert( (poi.cdf(0) - poi.probabilityOf(0)).abs < TOL,poi.cdf(0));
  }

}  
