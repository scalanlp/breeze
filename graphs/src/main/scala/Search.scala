package scalanlp.graphs


/**
 * Provide search routines for graphs. These are really graph traversals, but you
 * can break out early using Scala's breaks.
 *
 * @author dlwh
 */
trait Search {
  /**
   * Runs a depth-first traversal starting from source
   *
   * Use dfs(g,source).find(goalTest) to find a node, dfs(g,source).foreach(f) to just evaluate.
   */
  def dfs[N,E](g: Graph[N,E], source: N*):Traversable[N] = new Traversable[N] {
    def foreach[U](f:  N=>U) {
      val visited = collection.mutable.Set[N]();
      def rec(n: N) {
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

    override def toStream = {
      val res = new scala.collection.mutable.ArrayBuffer[N];
      foreach(res += _);
      res.toStream;
    }
  }

  /**
   * Runs a breadth-first traversal starting from source
   *
   * Use bfs(g,source).find(goalTest) to find a node, bfs(g,source).foreach(f) to just evaluate.
   */
  def bfs[N,E](g: Graph[N,E], source: N*): Traversable[N] = new Traversable[N] {
    def foreach[U](f: N=>U) {
      val visited = collection.mutable.Set[N]();
      val queue = new collection.mutable.Queue[N]();
      queue ++= source;
      while(!queue.isEmpty) {
        val n = queue.dequeue;
        if(!visited(n)) {
          f(n);
          visited += n;
          for(n2 <- g neighbors n if !visited(n2))
            queue += n2;
        }
      }
    }

    override def toStream = {
      val res = new scala.collection.mutable.ArrayBuffer[N];
      foreach(res += _);
      res.toStream;
    }
  }

}

object Search extends Search;