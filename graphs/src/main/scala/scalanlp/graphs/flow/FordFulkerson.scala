package scalanlp.graphs
package flow

import collection.mutable.HashMap

/**
 * 
 * @author dlwh
*/
object FordFulkerson {
  def findFlow[N,E](g: Digraph[N,E], capacity: E=>Int, source: N, target: N, pathFinder:PathFinder[N,E] = PathFinder[N,E]()): (N,N)=>Int = {
    val flow = new HashMap[N,HashMap[N,Int]] {
      override def default(k: N) = {
        val m = new HashMap[N,Int] {
          override def default(k: N) = 0;
        }
        update(k,m);
        m;
      }
    };

    var improved = true;
    while(improved) {
      improved = false;
      val residualGraph = g.filter { e =>
        capacity(e) > flow(g.source(e))(g.sink(e));
      }
      val path = pathFinder.findPath(residualGraph,source,target);
      if(path != Seq.empty) {
        improved = true;
        val minCost = (path.iterator.sliding(2).map { case Seq(n,m) => capacity(g.getEdge(n,m).get) }).min;
        for ( p@Seq(n,m) <- path.iterator.sliding(2)) {
          flow(n)(m) += minCost;
          flow(m)(n) -= minCost;
        }
      }
    }

    {(n: N, m: N) => flow.get(n).map(_(m)).getOrElse(0)}
  }

}

