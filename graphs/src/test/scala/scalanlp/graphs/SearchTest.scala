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
    assert( (bfs(g,'A).toSeq sameElements Seq(('A->'B,'B), ('A->'D,'D), ('B->'C,'C), ('D->'F,'F)))
            || (bfs(g,'A).toSeq sameElements Seq(('A->'D,'D), ('A->'B,'B), ('D->'F,'F), ('B->'C,'C) )))
    assert(bfs(g,'B).toSeq sameElements Seq( ('B->'C)-> 'C));
  }

 test("DFS gives expected results on a simple graph") {
    val g = Graph.fromEdges('A->'B,'B->'C,'A->'D,'D->'F);
   assert( (dfs(g,'A).toSeq sameElements Seq(('A->'B,'B),('B->'C,'C), ('A->'D,'D), ('D->'F,'F)))
           || (dfs(g,'A).toSeq sameElements Seq(('A->'D,'D),  ('D->'F,'F),('A->'B,'B), ('B->'C,'C) )))
    assert(dfs(g,'B).toSeq sameElements Seq( ('B->'C)->'C));
  }

  test("BFS find gives expected results on a simple graph") {
    val g = Graph.fromEdges('A->'B,'B->'C,'A->'D,'D->'F);
    assert( !bfs(g,'A).find(_._2 == 'D).isEmpty);
    assert( bfs(g,'B).find(_._2 == 'D).isEmpty);
  }

    test("DFS find gives expected results on a simple graph") {
    val g = Graph.fromEdges('A->'B,'B->'C,'A->'D,'D->'F);
    assert( !dfs(g,'A).find(_._2 == 'D).isEmpty);
    assert( dfs(g,'B).find(_._2 == 'D).isEmpty);
  }

  // from http://www.cs.utah.edu/~hal/courses/2009S_AI/Walkthrough/UCS/ucs.html
  test("UCS traversal") {
    val g = simpleWeightedDigraph;
    import scalanlp.math.Semiring.Tropical._;
    val trav = ucs(g,'A).takeWhile(_._2 != 'G).toSeq;
    assert(trav.map(_._2).sameElements(Seq('D,'E,'B,'F,'H)),trav);
  }

  // from http://www.cs.utah.edu/~hal/courses/2009S_AI/Walkthrough/UCS/ucs.html
  test("A* traversal with no heuristic") {
    val g = simpleWeightedDigraph;
    import scalanlp.math.Semiring.Tropical._;
    val trav = astar(g, (n:Symbol) => 0.0, 'A).takeWhile(_._2 != 'G).toSeq;
    assert(trav.map(_._2).sameElements(Seq('D,'E,'B,'F,'H)),trav);
  }

  // from http://www.cs.utah.edu/~hal/courses/2009S_AI/Walkthrough/UCS/ucs.html
  test("A* traversal with enough heuristic") {
    val g = simpleWeightedDigraph;
    def heuristic(n: Symbol) = n match {
      case 'A => 11.0
      case 'B => 6.0
      case 'C => 1.0
      case 'D => 9.0
      case 'E => 6.0
      case 'F => 3.0
      case 'G => 0.0
      case 'H => 4.0
      case _ => Double.PositiveInfinity;
    }
    import scalanlp.math.Semiring.Tropical._;
    val trav = astar(g, heuristic _, 'A).takeWhile(_._2 != 'G).toSeq;
    assert(trav.map(_._2).sameElements(Seq('D,'E,'F)),trav);
  }

  def simpleWeightedDigraph =  WeightedDigraphs.fromEdgeList(
    ('A,'B,7.),
    ('B,'C,5.),
    ('C,'G,1.),
    ('A,'D,2.),
    ('D,'E,3.),
    ('E,'F,3.),
    ('F,'G,3.),
    ('E,'H,4.0)
  );
}