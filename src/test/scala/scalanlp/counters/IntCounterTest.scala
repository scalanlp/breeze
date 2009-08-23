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

/*
import org.scalacheck._
import org.scalatest._;
import scalanlp.util.Implicits._;
import scalanlp.counters._;
import Counters._;
class IntCounterSuite extends JUnitSuite with Checkers {
  val arbitraryCounter = for(x <- Gen.listOf(Arbitrary.arbitrary[Int])) yield Counters.count(x);
  val arbitraryCounterPair = for(c1 <- arbitraryCounter; c2 <- arbitraryCounter) yield (c1,c2);
  val arbIntCounterPair = for(c1 <- arbitraryCounter; i <- Arbitrary.arbitrary[Int]) yield (c1,i)
  test("clear") {
    arbitraryCounter must pass  {(c : IntCounter[Int])  => c.clear(); c.size == 0 && c.total == 0}
    arbIntCounterPair must pass  {(cp : (IntCounter[Int],Int))  => 
      val (c,i) = cp;
      c.clear();
      c.get(i) == None && c(i) == 0 
    }
  }
  test("sum and clear") {
    arbitraryCounterPair must pass {(cp:(IntCounter[Int],IntCounter[Int])) =>
      val (c,c2) = cp;
      c += c2; 
      c.clear(); 
      c.size == 0 && c.total == 0
    }
  }
  test("sum preserves total") {
    arbitraryCounterPair must pass { (cp:(IntCounter[Int],IntCounter[Int]))  => 
      val (c,c2) = cp; 
      val expTotal = c.total + c2.total;
      val maxSize = c.size + c2.size; 
      c+=c2;
      expTotal == c.total && c.size <= maxSize
    }
  }

  test("scale preserves total") {
    check { (cp:(IntCounter[Int],Int))  => 
      val (c,i) = cp;
      (i == 0) ||  {
        val expTotal = c.total/i;
        c /=  i
        // truncation can cause weirdnesses.
        c.total.abs <= expTotal.abs;
      }
    }
    check { (cp:(IntCounter[Int],Int))  => 
      val (c,i) = cp;
      (i == 0) || {
        val expTotal = c.total*i;
        c *=  i
        c.total == expTotal;
      }
    }
  }
}
*/
