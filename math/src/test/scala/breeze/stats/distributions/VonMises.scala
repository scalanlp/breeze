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

import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._;

// VonMises variance depends on some reasonable handling of % 2 * pi, so we'll not include it.
@RunWith(classOf[JUnitRunner])
class VonMisesTest extends FunSuite with Checkers with UnivariateContinuousDistrTestBase with ExpFamTest[VonMises,Double] {
  import Arbitrary.arbitrary;

  val expFam = VonMises

  implicit def arbParameter = Arbitrary {
    for(mu <- arbitrary[Double].map{_.abs % (2 * math.Pi)}; // Gamma pdf at 0 not defined when shape == 1
        k <- arbitrary[Double].map {_.abs % 3.0 + 1.5}) yield (mu,k);
  }

  def paramsClose(p: (Double,Double), b: (Double,Double)) = {
    val y1 = (math.sin(p._1) - math.sin(b._1)).abs / (math.sin(p._1).abs / 2 + math.sin(b._1).abs / 2+ 1)  < 1E-1
    val y2 = (p._2 - b._2).abs / (p._2.abs / 2 + b._2.abs / 2+ 1)  < 1E-1
    y1 && y2
  }

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbDistr = Arbitrary {
    for(shape <- arbitrary[Double].map{x => math.abs(x) % (2 * math.Pi)};
        scale <- arbitrary[Double].map {x => math.abs(x) % 3.0 + 1.1}) yield new VonMises(shape,scale);
  }

  type Distr = VonMises

}  
