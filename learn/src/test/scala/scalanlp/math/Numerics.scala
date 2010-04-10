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

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

import Numerics._;

@RunWith(classOf[JUnitRunner])
class NumericsTest extends FunSuite with Checkers {
  import Arbitrary._;
  implicit def ae(x: Double) = new {
    def =~=(y: Double) = Math.abs(x-y)/x < 1E-6;
  }

  test("logsumming is approximately associative") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      logSum(a,logSum(b,c)) =~= logSum(logSum(a,b),c);
    })
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
      logSum(a,logSum(b,c)) =~= logSum(a,b,c);
    })
  }
 
  test("sum distributes over logsum") {
    check(Prop.forAll { (a: Double, b:Double, c: Double) =>
     (a + logSum(b,c)) =~= (logSum(a + b,a+c));
    })
  } 
}
