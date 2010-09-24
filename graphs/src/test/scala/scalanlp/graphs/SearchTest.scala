package scalanlp.graphs;

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
class SearchTest extends FunSuite with Checkers {

  test("BFS gives expected results on a simple graph") {
    val g = Graph.fromEdges('A->'B,'B->'C,'A->'D,'D->'F);
    assert( (bfs(g,'A).toSeq sameElements Seq('A, 'B, 'D, 'C, 'F))
            || bfs(g,'A).toSeq.sameElements(Seq('A, 'D, 'B, 'F, 'C)));
    assert(bfs(g,'B).toSeq sameElements Seq('B,'C));
  }

 test("DFS gives expected results on a simple graph") {
    val g = Graph.fromEdges('A->'B,'B->'C,'A->'D,'D->'F);
    assert(dfs(g,'A).toSeq.sameElements(Seq('A, 'B, 'C, 'D, 'F))
            || (dfs(g,'A).toSeq sameElements Seq('A, 'D, 'F, 'B, 'C)));
    assert(dfs(g,'B).toSeq sameElements Seq('B,'C));
  }

  test("BFS find gives expected results on a simple graph") {
    val g = Graph.fromEdges('A->'B,'B->'C,'A->'D,'D->'F);
    assert( !bfs(g,'A).find(_ == 'D).isEmpty);
    assert( bfs(g,'B).find(_ == 'D).isEmpty);
  }

    test("DFS find gives expected results on a simple graph") {
    val g = Graph.fromEdges('A->'B,'B->'C,'A->'D,'D->'F);
    assert( !dfs(g,'A).find(_ == 'D).isEmpty);
    assert( dfs(g,'B).find(_ == 'D).isEmpty);
  }
}