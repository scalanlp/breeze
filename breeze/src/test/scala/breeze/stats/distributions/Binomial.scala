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

import breeze.stats.DescriptiveStats._

@RunWith(classOf[JUnitRunner])
class BinomialTest extends FunSuite with Checkers with MomentsTestBase[Int] {
  import Arbitrary.arbitrary;


  override val numSamples: Int = 100000
  override val VARIANCE_TOLERANCE: Double = 1E-1

  implicit def arbDistr = Arbitrary {
    for(n <- arbitrary[Int].map{_.abs % 10000 + 1};
      p <- arbitrary[Double].map{_.abs % 1.0+1E-4}) yield new Binomial(n.abs+1,p)
  }

  def asDouble(x: Int) = x.toDouble
  def fromDouble(x: Double) = x.toInt
}
