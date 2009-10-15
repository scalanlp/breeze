package scalanlp.counters;

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
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import scalanlp.counters._;
import LogCounters._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class LogDoubleCounterTest extends FunSuite with Checkers {
  import Arbitrary._;
  implicit val arbitraryCounter : Arbitrary[LogDoubleCounter[Int]] = Arbitrary(for(x <- Gen.listOf(Arbitrary.arbitrary[(Int,Double)])) yield aggregate(x));

  test("log normalization") {
    check( Prop.forAll{ (cl: List[(Int,Double)]) =>
      val c = aggregate(cl);
      c.logTotal == Math.NEG_INF_DOUBLE || {
        val cn = LogCounters.normalize(c);
        val cn2 = LogCounters.normalize(c);
        (cn.total - 1.0).abs/c.logTotal < 1E-2 && c.forall { case (k,v) =>
          (Math.log(cn(k)) + c.logTotal - v).abs < 1E-6
        }
      }
    })
  }
}
