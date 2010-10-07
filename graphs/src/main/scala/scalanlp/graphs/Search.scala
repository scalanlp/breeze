package scalanlp.graphs

import scalanlp.math.Semiring


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
  def dfs[N,E](g: Graph[N,E], source: N, sources: N*):Iterable[N] = new Iterable[N] {
    def iterator:Iterator[N] = new Iterator[N] {
      val visited = collection.mutable.Set[N]();
      val stack = new collection.mutable.Stack[N]();
      stack.push(source);
      stack.pushAll(sources)

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
  def bfs[N,E](g: Graph[N,E], source: N, sources: N*): Iterable[N] = new Iterable[N] {
    def iterator:Iterator[N] = new Iterator[N] {
      val visited = collection.mutable.Set[N]();
      val queue = new collection.mutable.Queue[N]();
      queue += source;
      queue ++= sources;

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

  /**
   * Runs a uniform cost traversal.
   * Nodes are only visited once, so negative cycles are ignored.
   *
   * For this to make sense, the provided semiring must be idempotent i.e. (a + a) = a
   * If you want an actual traversal of all nodes including the full distance
   * costs for non-idempotent semirings, see Distance#SingleSourceShortestPaths
   */
  def ucs[N,E,W:Ordering:Semiring](g: WeightedGraph[N,E,W], source: N, sources: N*): Iterable[N] = new Iterable[N] {
    def iterator:Iterator[N] = new Iterator[N] {
      val visited = collection.mutable.Set[N]();
      val queue = new collection.mutable.PriorityQueue[(N,W)]()(Ordering[W].on((pair:(N,W)) => pair._2).reverse);
      for( src <- Iterator.single(source) ++ sources.iterator) {
        queue += (src -> Semiring[W].one);
      }

      override def hasNext = !queue.isEmpty;

      override def next = {
        val (n,w) = queue.dequeue();
        if(!visited(n)) {
          visited += n;
          for(e <- g.edgesFrom(n)) {
            val sink = g.endpoints(e).productIterator.find(n!=).get.asInstanceOf[N];
            val ew = g.weight(e);
            queue += (sink -> Semiring[W].times(w,ew));
          }
        }
        n;
      }

    }
  }

  /**
   * Runs an a* traversal with the given heuristic
   * Nodes are only visited once, so negative cycles are ignored.
   *
   * For this to make sense, the provided semiring must be idempotent i.e. (a + a) = a
   * If you want an actual traversal of all nodes including the full distance
   * costs for non-idempotent semirings, see Distance#SingleSourceShortestPaths
   */
  def astar[N,E,W:Ordering:Semiring](g: WeightedGraph[N,E,W], heuristic: N=>W, source: N, sources: N*): Iterable[N] = new Iterable[N] {
    def iterator:Iterator[N] = new Iterator[N] {
      val visited = collection.mutable.Set[N]();
      val ring = Semiring[W];
      import ring._;
      // (N,W,W) is (State,Cost so far, heuristic cost)
      val queue = new collection.mutable.PriorityQueue[(N,W,W)]()(Ordering[W].on((pair:(N,W,W)) => times(pair._2, pair._3)).reverse);
      for( src <- Iterator.single(source) ++ sources.iterator) {
        queue += ((src,Semiring[W].one,heuristic(src)));
      }

      override def hasNext = !queue.isEmpty;

      override def next = {
        val (n,w, _) = queue.dequeue();
        if(!visited(n)) {
          visited += n;
          for(e <- g.edgesFrom(n)) {
            val sink = g.endpoints(e).productIterator.find(n!=).get.asInstanceOf[N];
            val ew = g.weight(e);
            queue += ((sink,times(w,ew),heuristic(sink)));
          }
        }
        n;
      }

    }
  }

}

object Search extends Search;