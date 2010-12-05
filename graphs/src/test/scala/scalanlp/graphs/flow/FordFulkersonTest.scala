package scalanlp.graphs
package flow

/*
 Copyright 2010 David Hall

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
import scalanlp.graphs._;

@RunWith(classOf[JUnitRunner])
class FordFulkersonTest extends FunSuite with Checkers {
  test("Basic FF test"){
    val g = Digraph.fromEdges('A->'B, 'A->'C, 'B->'C,'B->'D,'C->'D);
    def capacity(e: (Symbol,Symbol)) = e match {
      case ('A,'B) => 1000;
      case ('B,'D) => 1000;
      case ('C,'D) => 1000;
      case ('B,'C) => 1;
      case ('A,'C) => 1000;
    }
    val flow = FordFulkerson.findFlow(g,capacity, 'A,'D);
    assert(flow('A,'B) === 1000);
    assert(flow('B,'C) === 0);
    assert(flow('B,'D) === 1000);
    assert(flow('C,'D) === 1000);
  }
}