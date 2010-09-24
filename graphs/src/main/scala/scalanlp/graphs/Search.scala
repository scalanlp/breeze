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
  def dfs[N,E](g: Graph[N,E], source: N*):Iterable[N] = new Iterable[N] {
    def iterator:Iterator[N] = new Iterator[N] {
      val visited = collection.mutable.Set[N]();
      val stack = new collection.mutable.Stack[N]();
      stack.pushAll(source);

      override def hasNext = !stack.isEmpty;

      override def next = {
        val n = stack.pop();
        if(!visited(n)) {
          visited += n;
          stack.pushAll(g.successors(n).filterNot(visited));
        }
        n
      }

    }
  }

  /**
   * Runs a breadth-first traversal starting from source
   *
   * Use bfs(g,source).find(goalTest) to find a node, bfs(g,source).foreach(f) to just evaluate.
   */
  def bfs[N,E](g: Graph[N,E], source: N*): Iterable[N] = new Iterable[N] {
    def iterator:Iterator[N] = new Iterator[N] {
      val visited = collection.mutable.Set[N]();
      val queue = new collection.mutable.Queue[N]();
      queue ++= source;

      override def hasNext = !queue.isEmpty;

      override def next = {
        val n = queue.dequeue();
        if(!visited(n)) {
          visited += n;
          queue ++= g.successors(n).filterNot(visited);
        }
        n;
      }

    }
  }

}

object Search extends Search;