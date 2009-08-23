package scalanlp.math;

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

import scalanlp.counters.Counters._;
import scalanlp.util.Implicits._;
import LogDouble._;

object LogDoubleSpecification extends Specification("LogDouble") with ScalaCheckMatchers {
  import Arbitrary._;
  val dPair = for { 
    d <- arbitrary[Double] suchThat {_ > 0};
    e <- arbitrary[Double] suchThat {_> 0}
  } yield {
    (d,e);
  }
  
  "addition" in {
    dPair must pass {(dp:(Double,Double)) => val (d,e) = dp; { (d.toLogDouble + e.toLogDouble).value =~= d + e}}
  }
  "subtraction" in {
    dPair must pass { (dp:(Double,Double)) => val (d,e) = dp;{ d < e || (d.toLogDouble - e.toLogDouble).value =~= d - e}}
  }
  "multiplication" in {
    dPair must pass { (dp:(Double,Double)) => val (d,e) = dp;{  (d.toLogDouble * e.toLogDouble).value =~= d * e}}
  }
  "division" in { 
    dPair must pass {(dp:(Double,Double)) => val (d,e) = dp;{ (d.toLogDouble / e.toLogDouble).value =~= d / e}}
  }
}

import org.specs.runner._;
class LogDoubleTest extends JUnit4(LogDoubleSpecification);
*/
