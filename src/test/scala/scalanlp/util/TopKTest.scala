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

package scalanlp.util;

import org.scalatest._;
import org.scalatest.junit._;
import org.junit.runner.RunWith;

import TopKImplicits._;

@RunWith(classOf[JUnitRunner])
class TopKTest extends FunSuite {
  test("Test we get proper output ordering") {
    val random = new java.util.Random();
    val values = Array.tabulate(1000)(i => random.nextDouble);
    for (i <- 0 until 100) {
      val sampled = Array.tabulate(1000)(i => values(random.nextInt(values.length))).toList;
      expect(sampled.sortWith(_ > _).take(20).toList)(sampled.topk(20).toList);
    }
  }

  test("Test we get proper output ordering if we don't have enough") {
    val random = new java.util.Random();
    val values = Array.tabulate(10)(i => random.nextDouble);
    for (i <- 0 until 100) {
      val sampled = Array.tabulate(10)(i => values(random.nextInt(values.length))).toList;
      expect(sampled.sortWith(_ > _).take(20).toList)(sampled.topk(20).toList);
    }
  }

}
