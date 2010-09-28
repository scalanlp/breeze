package scalanlp.graphs

import scalanlp.math.Semiring;
import scala.collection.mutable.HashMap;
import scala.collection.mutable.HashSet;

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
 * Provides routines for computing the distance between
 * nodes in a graph.
 *
 * @author dlwh
 */
object Distance {
  /**
   * For suitable graphs, return the sum of all paths from the start states
   * to each state.
   *

   * Implements Generic-Single-Source-Shortest-Distance described in Mohri(2002)
   * with extra support for doing closure operations on selfloops. Only works
   * for acyclic graphs, k-closed semirings, or graphs that are acyclic except
   * for self-loops. May converge anyway, eventually.
   */
  def singleSourceShortestDistances[W:Semiring, N, E](graph: WeightedDigraph[N,E,W], source: N*):Map[N,W] = {
    val ring = implicitly[Semiring[W]];
    import ring._;

    val d = new HashMap[N,W];
    val r = new HashMap[N,W];
    val selfLoops = new HashMap[N,W];

    val S = new collection.mutable.Queue[N]();
    val visited = new HashMap[N,Int];
    val enqueued = new HashSet[N];
    for( s <- source) {
      d(s) = plus(zero,one);
      r(s) = plus(zero,one);
      S += s;
      enqueued(s) = true;
    }

    while(!S.isEmpty) {
      val from = S.head;
      S.dequeue();
      enqueued -= from;

      visited(from) = visited.getOrElse(from,0)+1;

      if(visited(from) == 1) {
        val loopWeight = graph.edgeWeight(from, from);
        selfLoops(from) = closure(loopWeight.getOrElse(zero));
      } else if(visited(from) % 20 == 0) {
        println("Visited " + from + " " + visited(from) + " times!");
      }
      val dkk_star = selfLoops(from);

      // relax the edge, expand its new incoming mass
      r(from) = times(r(from),dkk_star);

      val rFrom = r(from);
      r -= from;

      for(e <- graph.edgesFrom(from)) {
        val to = graph.sink(e);
        val w = graph.weight(e);
        if (!closeTo(w,zero) && from != to) {
          val dt = d.getOrElse(to,zero);
          val wRFrom = times(rFrom,w);
          val (dt_p_wRFrom,tooCloseToMatter) = maybe_+=(dt,wRFrom);
          if( !tooCloseToMatter) {
            r(to) = maybe_+=(r.getOrElse(to,zero),wRFrom)._1;
            d(to) = dt_p_wRFrom;
            if(!enqueued(to)) {
              S += to;
              enqueued += to;
            }
          }
        }
      }
    }

    for(  (s,mass) <- selfLoops if !closeTo(mass,zero)) {
      d(s) = times(d(s),mass);
    }

    Map.empty ++ d;
  }



  /**
   * Implements Gen-All-Pairs described in Mohri (2002).
   * Finds all pair-wise distances between all points in O(n^3),
   * where n is the number of states. Works for any complete semiring.
   *
   * This is a generalization of Floyd-Warshall
   */
  def allPairDistances[W:Semiring, N, E](graph: WeightedDigraph[N,E,W]) = {
    val ring = implicitly[Semiring[W]];
    import ring._;
    val distances = neighborDistances(graph)

    for {
      k <- graph.nodes
    } {
      // cache some commonly used values
      val dkk = distances(k)(k);
      val dkkStar = closure(dkk);

      for {
        (j,dkj) <- distances(k).iterator
        if j != k && !closeTo(dkj,zero)
        i <- graph.nodes if i != k
        dik = distances(i)(k)
        if !closeTo(dik,zero)
      } {
        val current = distances(i)(j);
        val pathsThroughK = times(dik,times(dkkStar,dkj));
        distances(i)(j) = maybe_+=(current,pathsThroughK)._1;
      }

      for (i <- graph.nodes if i != k) {
        distances(k)(i) = times(dkkStar,distances(k)(i));
        distances(i)(k) = times(distances(i)(k),dkkStar);
      }
      distances(k)(k) = dkkStar;
    }

    Map.empty ++ distances.map { case (from,map) =>
      (from,Map.empty ++ map  withDefaultValue zero)
    } withDefaultValue (Map.empty.withDefaultValue(zero))

  }

  /*
  * Returns the distances between individual pairs of states using
  * only one hop
  */
  private def neighborDistances[W:Semiring, N, E](graph: WeightedDigraph[N,E,W]) = {
    val ring = implicitly[Semiring[W]];
    import ring._;

    val distances = new collection.mutable.HashMap[N,collection.mutable.HashMap[N,W]] {
      override def default(k: N) = {
        val res = new HashMap[N,W] {
          override def default(k: N) = zero;
        }
        update(k,res);
        res
      }
    }
    graph.edges.foreach { e =>
      val (from,to) = graph.endpoints(e);
      val w = graph.weight(e);
      val current = distances(from)(to);
      distances(from)(to) = maybe_+=(current,w)._1;
    }
    distances
  }

}