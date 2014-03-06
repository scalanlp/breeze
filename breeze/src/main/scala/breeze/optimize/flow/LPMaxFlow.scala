package breeze.optimize.flow

import breeze.optimize.linear.LinearProgram
import collection.mutable.ArrayBuffer

/**
 * 
 * @author dlwh
 */

class LPMaxFlow[N](val g: FlowGraph[N]) {
  import g._
  lazy val maxFlow:(Map[Edge,Double],Double) = {
    val queue = collection.mutable.Queue[N]()
    queue += g.source
    val visited = collection.mutable.Set[N]()

    val lp = new LinearProgram
    import lp._


    val constraints = new ArrayBuffer[Constraint]()
    val incoming = new collection.mutable.HashMap[N,ArrayBuffer[Expression]]() {
      override def default(k: N) = {
        val a = new ArrayBuffer[Expression]()
        update(k,a)
        a
      }
    }
    val outgoing = new collection.mutable.HashMap[N,ArrayBuffer[Expression]]() {
      override def default(k: N) = {
        val a = new ArrayBuffer[Expression]()
        update(k,a)
        a
      }
    }

    val edgeMap = collection.mutable.Map[Edge,Variable]()

    while(queue.nonEmpty) {
      val n = queue.dequeue()
      if(!visited(n)) {
        visited += n
        if(n != sink)
          for(e <- edgesFrom(n)) {
            val f_e = Real(e.head + "->" + e.tail)
            edgeMap += (e -> f_e)
            constraints += (f_e <= e.capacity)
            constraints += (f_e  >= 0.0 )
            incoming(e.tail) += f_e * e.gain
            outgoing(e.head) += f_e
            if(!visited(e.tail))
              queue += e.tail
          }
      }
    }

    for(n <- visited; inc <- incoming.get(n); out <- outgoing.get(n)) {
      constraints += (inc.reduceLeft(_ + _) <= out.reduceLeft(_ + _))
      constraints += ( out.reduceLeft(_ + _) <= inc.reduceLeft(_ + _) )
    }

    val total = incoming(sink).reduceLeft( _ + _)
    val solution = maximize { total subjectTo(constraints:_*) }

    (Map.empty ++ edgeMap.mapValues(solution.valueOf(_)),solution.value)
  }

  def minCostFlow(minimumFlow: Double = -1) = {
    val mf = if(minimumFlow < 0) maxFlow._2 else minimumFlow


    val queue = collection.mutable.Queue[N]()
    queue += g.source
    val visited = collection.mutable.Set[N]()

    val lp = new LinearProgram
    import lp._

    val costs = new ArrayBuffer[Expression]()

    val constraints = new ArrayBuffer[Constraint]()
    val incoming = new collection.mutable.HashMap[N,ArrayBuffer[Expression]]() {
      override def default(k: N) = {
        val a = new ArrayBuffer[Expression]()
        update(k,a)
        a
      }
    }
    val outgoing = new collection.mutable.HashMap[N,ArrayBuffer[Expression]]() {
      override def default(k: N) = {
        val a = new ArrayBuffer[Expression]()
        update(k,a)
        a
      }
    }

    val edgeMap = collection.mutable.Map[Edge,Variable]()

    while(queue.nonEmpty) {
      val n = queue.dequeue()
      if(!visited(n)) {
        visited += n
        if(n != sink)
          for(e <- edgesFrom(n)) {
            val f_e = Real(e.head + "->" + e.tail)
            edgeMap += (e -> f_e)
            constraints += (f_e <= e.capacity)
            constraints += (f_e >= 0.0 )
            costs += (f_e * e.cost)

            incoming(e.tail) += f_e * e.gain
            outgoing(e.head) += f_e
            if(!visited(e.tail))
              queue += e.tail
          }
      }
    }

    for(n <- visited; inc <- incoming.get(n); out <- outgoing.get(n)) {
      constraints += (inc.reduceLeft(_ + _) <= out.reduceLeft(_ + _))
      constraints += ( out.reduceLeft(_ + _) <= inc.reduceLeft(_ + _) )
    }

    val flowTotal = incoming(sink).reduceLeft(_ + _)
    constraints += (flowTotal >= mf)

    val total = costs.reduceLeft( _ + _)
    val solution = maximize { total * -1.0 subjectTo(constraints:_*) }

    (Map.empty ++ edgeMap.mapValues(solution.valueOf(_)),-solution.value)
  }
}
