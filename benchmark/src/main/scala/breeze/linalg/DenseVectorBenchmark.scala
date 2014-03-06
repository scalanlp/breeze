package breeze.benchmark.linalg

import breeze.benchmark._

import breeze.linalg._
import breeze.stats.distributions._

object DenseVectorBenchmark extends MyRunner(classOf[DenseVectorBenchmark])

trait BuildsRandomVectors {
  private val uniform = Uniform(0,1)
  def randomArray(size: Int): DenseVector[Double] = {
    val result = DenseVector.zeros[Double](size)
    var i=0;
    while (i < size) {
      result.update(i, uniform.draw())
      i += 1
    }
    result
  }
}

class DenseVectorBenchmark extends BreezeBenchmark with BuildsRandomVectors {
  def timeAllocate(reps: Int) = run(reps) {
    DenseVector.zeros[Double](1024)
  }
  def timeFill(reps: Int) = run(reps) {
    DenseVector.fill[Double](1024, 23)
  }

  private val applyArray = randomArray(1024*8)
  def timeUnsafeValueAt(reps: Int) = run(reps) {
    var i=0;
    var t: Double = 0
    while (i < applyArray.size) {
      t += applyArray.unsafeValueAt(i) //This is not strictly part of the benchmark, but done so that the JIT doensn't eliminate everything
      i += 1
    }
    t
  }

  def timeApply(reps: Int) = run(reps) {
    var i=0;
    var t: Double = 0
    while (i < applyArray.size) {
      t += applyArray(i) //This is not strictly part of the benchmark, but done so that the JIT doensn't eliminate everything
      i += 1
    }
    t
  }
}
