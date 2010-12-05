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
  def dfs[N,E](g: Graph[N,E], source: N, sources: N*): Iterable[(E,N)] = new Iterable[(E,N)] {
    def iterator:Iterator[(E,N)] = new Iterator[(E,N)] {
      val visited = collection.mutable.Set[N]();
      val stack = new collection.mutable.Stack[(E,N)]();
      visited += source;
      visited ++= sources;
      stack.pushAll {
        g.edgesFrom(source).map { e => e -> nextNode(g,e,source) }
      }
      stack.pushAll {
        for( s <- sources.iterator; e <- g.edgesFrom(s)) yield (e -> nextNode(g,e,s));
      }

      override def hasNext = !stack.isEmpty;

      override def next = {
        val p@(e,n) = stack.pop();
        if(!visited(n)) {
          visited += n;
          for(e <- g.edgesFrom(n)) {
            stack.push(e -> nextNode(g,e,n))
          }
        }
        p;
      }

    }
  }


  /**
   * Runs a breadth-first traversal starting from source, returning an iterable of (Node,Incident Edge) that
   * are visited. No node will be visited more than once.
   *
   * Use bfs(g,source).find(goalTest) to find a node, bfs(g,source).foreach(f) to just evaluate.
   */
  def bfs[N,E](g: Graph[N,E], source: N, sources: N*): Iterable[(E,N)] = new Iterable[(E,N)] {
    def iterator:Iterator[(E,N)] = new Iterator[(E,N)] {
      val visited = collection.mutable.Set[N]();
      val queue = new collection.mutable.Queue[(E,N)]();
      visited += source;
      visited ++= sources;
      queue ++= {
        g.edgesFrom(source).map { e => e -> nextNode(g,e,source) }
      }
      queue ++=  {
        for( s <- sources.iterator; e <- g.edgesFrom(s)) yield (e -> nextNode(g,e,s));
      }

      override def hasNext = !queue.isEmpty;

      override def next = {
        val p@(e,n) = queue.dequeue();
        if(!visited(n)) {
          visited += n;
          for(e <- g.edgesFrom(n)) {
            queue += (e -> nextNode(g,e,n))
          }
        }
        p;
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
  def ucs[N,E,W:Ordering:Semiring](g: WeightedGraph[N,E,W], source: N, sources: N*): Iterable[(E,N)] = new Iterable[(E,N)] {
    def iterator:Iterator[(E,N)] = new Iterator[(E,N)] {
      val visited = collection.mutable.Set[N]();
      val queue = new collection.mutable.PriorityQueue[(E,N,W)]()(Ordering[W].on((pair:(E,N,W)) => pair._3).reverse);
      for( src <- Iterator.single(source) ++ sources.iterator) {
        visited += src;
        queue ++= g.edgesFrom(src).map{e => (e,nextNode(g,e,src),g.weight(e))}
      }

      override def hasNext = !queue.isEmpty;

      override def next = {
        val (e,n,w) = queue.dequeue();
        if(!visited(n)) {
          visited += n;
          for(e <- g.edgesFrom(n)) {
            val sink = nextNode(g,e,n);
            val ew = g.weight(e);
            queue += ((e,sink,Semiring[W].times(w,ew)));
          }
        }
        e-> n;
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
  def astar[N,E,W:Ordering:Semiring](g: WeightedGraph[N,E,W], heuristic: N=>W, source: N, sources: N*): Iterable[(E,N)] = new Iterable[(E,N)] {
    def iterator:Iterator[(E,N)] = new Iterator[(E,N)] {
      val visited = collection.mutable.Set[N]();
      val ring = Semiring[W];
      import ring._;
      // (N,W,W) is (State,Cost so far, heuristic cost)
      val queue = new collection.mutable.PriorityQueue[(E,N,W,W)]()(Ordering[W].on((pair:(E,N,W,W)) => times(pair._3, pair._4)).reverse);
      for( src <- Iterator.single(source) ++ sources.iterator) {
        visited += src;
        queue ++= g.edgesFrom(src).map{e =>
          val sink = nextNode(g,e,src);
          (e,sink,g.weight(e),heuristic(sink))
        }
      }

      override def hasNext = !queue.isEmpty;

      override def next = {
        val (e, n,w, _) = queue.dequeue();
        if(!visited(n)) {
          visited += n;
          for(e <- g.edgesFrom(n)) {
            val sink = nextNode(g,e,n);
            val ew = g.weight(e);
            queue += ((e,sink,Semiring[W].times(w,ew), heuristic(sink)));
          }
        }
        e -> n;
      }

    }
  }

  private def nextNode[N,E](g: Graph[N,E], e: E, s: N) = g match {
    case dg: Digraph[N,E] => dg.sink(e);
    case _ =>
      val (n1,n2) = g.endpoints(e);
      if(n1 == s) n2 else n1;
  }

}

object Search extends Search;