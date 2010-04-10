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
import Counters._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class DoubleCounterTest extends FunSuite with Checkers {
  import Arbitrary._;
  implicit val arbitraryCounter : Arbitrary[DoubleCounter[Int]] = Arbitrary(for(x <- Gen.listOf(Arbitrary.arbitrary[(Int,Double)])) yield Counters.aggregate(x));

  test("copy is a deep copy") {
    check(Prop.forAll { (c:DoubleCounter[Int]) =>
      val oldSize = c.size;
      val oldTotal = c.total;
      val oldElems = Map() ++ c.iterator;
      val c2 = c.copy;
      c2 :-= c2;
      c.size == oldSize && c.total == oldTotal;
    });
  }

  test("Adding to a counter makes it contain it") {
    check(Prop.forAll { (c: DoubleCounter[Int], a: Int) =>
      val c2 = c.copy;
      c2.incrementCount(a,1);
      c2.contains(a) && c2.activeDomain.contains(a);
    });
  }

  test("sum preserves total") {
    check( Prop.forAll { (cp:(DoubleCounter[Int],DoubleCounter[Int]))  => 
      val (c,c2) = cp; 
      val expTotal = c.total + c2.total;
      val maxSize = c.size + c2.size; 
      c+=c2;
      (expTotal - c.total).abs < 1E-6 && c.size <= maxSize
    })
  }

  test("scale preserves total") {
    check ( Prop.forAll { (cp:(DoubleCounter[Int],Double))  => 
      val (c2,i) = cp;
      val c = c2.copy;

      (i == 0) ||  {
        val expTotal = c.total/i;
        c /=  i
        (c.total - expTotal).abs < 1E-6;
      }
    })
    check ( Prop.forAll { (cp:(DoubleCounter[Int],Double))  => 
      val (c2,i) = cp;
      val c = c2.copy;
      (i == 0) || {
        val expTotal = c.total*i;
        c *=  i
        (c.total - expTotal).abs < 1E-6;
      }
    })
  }
}
