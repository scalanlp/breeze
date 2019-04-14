package breeze.optimize.flow

import org.scalatest._

class LPMaxFlowTest extends FunSuite {

  test("LPMaxFlowTest") {
    val g = new FlowGraph[Int] {

      def source = 0

      def sink = 5

      case class E(head: Int, tail: Int, override val capacity: Double, override val cost: Double) extends Edge

      val edges = Map(
        0 -> Seq(E(0, 1, 3, 3), E(0, 2, 3, 1)),
        1 -> Seq(E(1, 3, 2, 1), E(1, 4, 2, 1)),
        2 -> Seq(E(2, 3, 1, 4), E(2, 4, 2, 2)),
        3 -> Seq(E(3, 5, 2, 2)),
        4 -> Seq(E(4, 5, 2, 1)))

      def edgesFrom(n: Int) = edges(n).iterator
    }

    val lpm = new LPMaxFlow(g)
    assert((lpm.maxFlow._2 - 4).abs < 1E-5, lpm)
    assert((lpm.minCostFlow()._2 - 20).abs < 1E-5, lpm)
  }
}
