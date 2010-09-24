package scalanlp.graphs
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

/**
 * Provides views on graphs.
 * @author dlwh
 */
trait Transformations {
  def reverse[Node,Edge](g: Graph[Node,Edge])(implicit reverser: EdgeReverser[Edge]): Graph[Node,Edge] = new Graph[Node,Edge] {
    def edges = g.edges.map(reverser);
    def endpoints(e: Edge) = g.endpoints(reverser(e));
    def nodes = g.nodes;
    def edgesTouching(n: Node) = g.edgesTouching(n).map(reverser);
    def successors(n: Node) = g.successors(n);
    def getEdge(n1: Node, n2: Node) = g.getEdge(n2,n1) map reverser;
  }


  def reverse[Node,Edge](g: Digraph[Node,Edge])(implicit reverser: EdgeReverser[Edge]):Digraph[Node,Edge] = new Digraph[Node,Edge] {
    def edges = g.edges.map(reverser);
    override def endpoints(e: Edge) = g.endpoints(e).swap;
    def nodes = g.nodes;
    override def edgesTouching(n: Node) = g.edgesTouching(n).map(reverser);
    def successors(n: Node) = g.successors(n);
    def getEdge(n1: Node, n2: Node) = g.getEdge(n2,n1) map reverser;

    def source(e: Edge) = g.sink(e);
    def sink(e: Edge) = g.source(e);
    def edgesFrom(n: Node):Iterator[Edge] = g.edgesTo(n).map(reverser);
    def edgesTo(n: Node) = g.edgesFrom(n).map(reverser);
  }

}

trait EdgeReverser[Edge] extends (Edge=>Edge);

object EdgeReverser {
  implicit def revPair[N]= new EdgeReverser[(N,N)] {
    def apply(e: (N,N)) = e.swap;
  }
}
