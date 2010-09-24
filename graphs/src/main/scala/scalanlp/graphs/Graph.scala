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
 * Represents a graph, which consists of Nodes, Edges, and ways to go from Edges to Nodes, and vice-versa.
 */
trait Graph[Node,Edge] {
  def edges: Iterator[Edge];
  def endpoints(e: Edge):(Node,Node);
  def nodes: Iterable[Node];
  def edgesTouching(n: Node): Iterator[Edge];
  def neighbors(n: Node): Iterator[Node]
  def getEdge(n1: Node, n2: Node): Option[Edge];
}

/**
 * A Digraph refines the notion of a graph, adding sources and sinks to edges.
 */
trait Digraph[Node,Edge] extends Graph[Node,Edge] {
  def source(e: Edge): Node
  def sink(e: Edge): Node

  def edgesFrom(n: Node): Iterator[Edge];
  def edgesTo(n: Node): Iterator[Edge];
  def edgesTouching(n: Node) = edgesFrom(n) ++ edgesTo(n);
}

object Graph {
  /**
   * Constructs a graph from an edge list. The edgelist is the primary data
   * structure, so it's not recommended.
   */
  def fromEdges[N](edges: (N,N)*):Graph[N,(N,N)] = Digraph.fromEdges(edges:_*);
  /**
   * Constructs a graph from an adjacency list. The list must be symmetric.
   */
  def fromAdjacencyList[N](adjacencyList: Map[N,Seq[N]]): Graph[N,(N,N)] = {
    Digraph.fromAdjacencyList(adjacencyList);
  }

}


object Digraph {

  /**
   * Constructs a graph from an edge list.
   */
  def fromEdges[N](edges: (N,N)*) = {
    val adjList = edges.groupBy(_._1).mapValues(_.map(_._2));
    Digraph.fromAdjacencyList(adjList);
  }

  /**
   * Constructs a graph from an adjacency list.
   */
  def fromAdjacencyList[N](adjacencyList: Map[N,Seq[N]]):Digraph[N,(N,N)] = {
    val reversed: IndexedSeq[(N,N)] = (for( (n,adj) <- adjacencyList.iterator; m <- adj.iterator) yield (n,m)).toIndexedSeq;
    val groupedReversed = reversed.groupBy(_._1).mapValues(_ map (_._2));
    type Node = N;
    type Edge = (N,N);
    new Digraph[N,(N,N)] {

      def edges = for( (n,adj) <- adjacencyList iterator; m <- adj iterator) yield (n,m);
      def endpoints(e: Edge):(Node,Node) = e;
      lazy val nodes = (adjacencyList.keys ++ adjacencyList.values.flatten).toSet
      def neighbors(n: Node) = {
        val toNeighbors = adjacencyList.getOrElse(n, Seq.empty)
        val fromNeighbors = groupedReversed.getOrElse(n, Seq.empty)
        (toNeighbors.toSet ++ fromNeighbors).iterator
      }
      def getEdge(n1: Node, n2: Node) = {
        for(adj <- adjacencyList.get(n1); m <- adj.find(_ == n2)) yield (n1,n2);
      }
      def source(e: Edge): Node = e._1
      def sink(e: Edge): Node = e._2;

      def edgesFrom(n: Node): Iterator[Edge] = adjacencyList.getOrElse(n,Seq.empty).iterator.map(n2 => (n,n2));
      def edgesTo(n: Node): Iterator[Edge] = groupedReversed.getOrElse(n,Seq.empty).iterator.map(n2 => (n2,n));

      override def toString() = "Graph[" + adjacencyList + "]";
    }
  }
}