package scalanlp.graphs


/**
 * Provide search routines for graphs. These are really graph traversals, but you
 * can break out early using Scala's breaks.
 *
 * @author dlwh
 */
object Search {
  /**
   * Runs a depth-first traversal starting from source
   */
  def dfs(g: Graph)(source: g.Node*)(f: (g.Node=>Unit)) = {
    val visited = collection.mutable.Set[g.Node]();
    def rec(n: g.Node) {
      if(!visited(n)) {
        visited += n;
        f(n);
        for(n2 <- g.neighbors(n)) {
          rec(n2);
        }
      }
    }

    source foreach rec;
  }

  /**
   * Searches for a goal using the provided goal test via depth-first search.
   */
  def dfs(g: Graph)(source: g.Node*)(goal: (g.Node=>Boolean)):Option[g.Node] = {
    dfs(g)(source:_*) { n=>
      if(goal(n)) return Some(n);
    }
    None;
  }

  /**
   * Runs a breadth-first traversal starting from source
   */
  def bfs(g: Graph)(source: g.Node*)(f: (g.Node=>Unit)) = {
    val visited = collection.mutable.Set[g.Node]();
    val queue = new collection.mutable.Queue[g.Node]();
    queue ++= source;
    while(!queue.isEmpty) {
      val n = queue.dequeue;
      f(n);
      queue ++= g.neighbors(n);
    }
  }

   /**
   * Searches for a goal using the provided goal test via breadth-first search.
   */
  def bfs(g: Graph)(source: g.Node*)(goal: (g.Node=>Boolean)):Option[g.Node] = {
    bfs(g)(source:_*) { n=>
      if(goal(n)) return Some(n);
    }
    None;
  }


}