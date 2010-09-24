package scalanlp;

/**
 * 
 * @author dlwh
 */
package object graphs extends scalanlp.graphs.Search with scalanlp.graphs.Transformations {
  import scalanlp.graphs._;
  type WeightedGraph[Node,Edge,Weight] = Graph[Node,Edge] with Weighted[Edge,Weight]
  type WeightedDigraph[Node,Edge,Weight] = Digraph[Node,Edge] with Weighted[Edge,Weight]
}