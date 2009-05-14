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
import org.specs._;
import org.specs.matcher._;
import scalanlp.util.Implicits._;

/*
object IntCounterSpec extends Specification("IntCounter") with ScalaCheckMatchers {
  val arbitraryCounter = for(x <- Gen.listOf(Arbitrary.arbitrary[Int])) yield Counters.count(x);
  val arbitraryCounterPair = for(c1 <- arbitraryCounter; c2 <- arbitraryCounter) yield (c1,c2);
  val arbIntCounterPair = for(c1 <- arbitraryCounter; i <- Arbitrary.arbitrary[Int]) yield (c1,i)
  "clear" in {
    arbitraryCounter must pass  {(c : IntCounter[Int])  => c.clear(); c.size == 0 && c.total == 0}
    arbIntCounterPair must pass  {(cp : (IntCounter[Int],Int))  => 
      val (c,i) = cp;
      c.clear();
      c.get(i) == None && c(i) == 0 
    }
  }
  "sum and clear" in {
    arbitraryCounterPair must pass {(cp:(IntCounter[Int],IntCounter[Int])) =>
      val (c,c2) = cp;
      c += c2; 
      c.clear(); 
      c.size == 0 && c.total == 0
    }
  }
  "sum preserves total" in {
    arbitraryCounterPair must pass { (cp:(IntCounter[Int],IntCounter[Int]))  => 
      val (c,c2) = cp; 
      val expTotal = c.total + c2.total;
      val maxSize = c.size + c2.size; 
      c+=c2;
      expTotal == c.total && c.size <= maxSize
    }
  }

  "normalize has total 1" in {
    arbitraryCounter must pass { (c:(IntCounter[Int]))  =>
      c.total == 0 || c.normalized.total =~= 1.0
    }
  }

  "scale preserves total" in {
    arbIntCounterPair must pass { (cp:(IntCounter[Int],Int))  => 
      val (c,i) = cp;
      (i == 0) ||  {
        val expTotal = c.total/i;
        c /=  i
        // truncation can cause weirdnesses.
        c.total.abs <= expTotal.abs;
      }
    }
    arbIntCounterPair must pass { (cp:(IntCounter[Int],Int))  => 
      val (c,i) = cp;
      (i == 0) || {
        val expTotal = c.total*i;
        c *=  i
        c.total == expTotal;
      }
    }
  }
}

import org.specs.runner._;
class IntCounterTest extends JUnit4(IntCounterSpec);
*/
