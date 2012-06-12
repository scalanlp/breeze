package breeze.graphs
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
 * Provides transformations on graphs.
 * @author dlwh
 */
trait Transformations {
  def reverse[Node,Edge](g: Digraph[Node,Edge])(implicit reverser: EdgeReverser[Edge]):Digraph[Node,Edge] = {
    val reversedEdges = g.edges.map(reverser).toIndexedSeq;
    val groupedBySource = reversedEdges.groupBy(g.source _);
    new Digraph[Node,Edge] {
      def edges = reversedEdges.iterator;
      def nodes = g.nodes;
      def endPoints(e: Edge) = g.endpoints(e).swap;
      def edgesFrom(n: Node) = groupedBySource.getOrElse(n, Seq.empty).iterator;
      def successors(n: Node) = groupedBySource.getOrElse(n, Seq.empty).map(sink).toSet.iterator;
      def getEdge(n: Node, n2: Node) = groupedBySource.get(n).flatMap(_.find(e => sink(e) == n2));
      // edges are already reversed, so use actual source/sink
      def sink(e: Edge) = g.sink(e);
      def source(e: Edge) = g.source(e);
    }
  }


  def reverseWeighted[Node,Edge,W](g: Weighted[Node,Edge,W] with Digraph[Node,Edge])(implicit reverser: EdgeReverser[Edge]): WeightedDigraph[Node,Edge,W] = {
    val reversedEdges = g.edges.map(reverser).toIndexedSeq;
    val groupedBySource = reversedEdges.groupBy(g.source _);
    new Digraph[Node,Edge]  with Weighted[Node,Edge,W] {
      def edges = reversedEdges.iterator;
      def nodes = g.nodes;
      def endPoints(e: Edge) = g.endpoints(e).swap;
      def edgesFrom(n: Node) = groupedBySource.getOrElse(n, Seq.empty).iterator;
      def successors(n: Node) = groupedBySource.getOrElse(n, Seq.empty).map(sink).toSet.iterator;
      def getEdge(n: Node, n2: Node) = groupedBySource.get(n).flatMap(_.find(e => sink(e) == n2));
      // edges are already reversed, so use actual source/sink
      def sink(e: Edge) = g.sink(e);
      def source(e: Edge) = g.source(e);
      def weight(e: Edge) = g.weight(e);
    }
  }

}

trait EdgeReverser[Edge] extends (Edge=>Edge);

object EdgeReverser {
  implicit def revPair[N]= new EdgeReverser[(N,N)] {
    def apply(e: (N,N)) = e.swap;
  }
  implicit def revWeightedEdge[N,W]= new EdgeReverser[(N,N,W)] {
    def apply(e: (N,N,W)) = {
      val (n,m,w) = e;
      (m,n,w);
    }
  }
}
