package breeze.linalg.support

import breeze.benchmark._
import breeze.linalg._
import breeze.stats.distributions._
import CanTraverseValues.ValuesVisitor

object CanTraverseValuesBenchmark extends MyRunner(classOf[CanTraverseValuesBenchmark])



class CanTraverseValuesBenchmark extends BreezeBenchmark with BuildsRandomVectors {

  def timeSumWithCanTraverseValues(reps: Int) = runWith(reps, { randomArray(1024*8) })(arr => {
    val visitor = new ValuesVisitor[Double] {
      var sum: Double = 0
      @inline
      def visit(a: Double) = { sum += a }
      def zeros(numZero: Int, zeroValue: Double) = ()
    }
    DenseVector.canIterateValues[Double].traverse(arr, visitor)
    visitor.sum
  })

}
