package breeze.optimize.flow

/**
 * 
 * @author dlwh
 */
trait FlowGraph[N] {
  def source: N
  def sink: N
  def edgesFrom(n: N):Iterator[Edge]
  trait Edge {
    def capacity: Double = Double.PositiveInfinity
    def cost: Double = 0.0
    def gain: Double = 1.0
    def head: N
    def tail: N
  }
}
