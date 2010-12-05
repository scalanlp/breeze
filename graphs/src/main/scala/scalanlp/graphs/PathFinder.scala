package scalanlp.graphs

/**
 * 
 * @author dlwh
 */
trait PathFinder[N,E] {
  def findPath(g: Graph[N,E], s: N, t: N):Seq[N]
}

object PathFinder {
  def apply[N,E](trav: (Graph[N,E], N, N*)=>Iterable[(E,N)] = bfs[N,E] _) = fromTraversal(trav);

  def fromTraversal[N,E](trav: (Graph[N,E], N, N*)=>Iterable[(E,N)]):PathFinder[N,E] = new PathFinder[N,E] {
    def findPath(g: Graph[N,E], s: N, t: N) = {
      val pathTo = collection.mutable.HashMap[N,List[N]]();
      var found = false;
      for( (e:E,n:N) <- trav(g,s).iterator takeWhile(x => !found)) {
        val source = sourceNode(g,e,n)
        pathTo(n) = source :: pathTo.getOrElse(source,Nil);
        if(n == t) found = true;
      }

      if(found) (t::pathTo(t)).reverse;
      else Seq.empty

    }
  }

  private def sourceNode[N,E](g: Graph[N,E], e: E, s: N) = g match {
    case dg: Digraph[N,E] => dg.source(e);
    case _ =>
      val (n1,n2) = g.endpoints(e);
      if(n1 == s) n2 else n1;
  }

}
