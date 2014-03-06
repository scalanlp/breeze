package breeze.linalg

import breeze.benchmark._

import breeze.linalg._
import breeze.stats.distributions._

object DenseVectorBenchmark extends MyRunner(classOf[DenseVectorBenchmark])

trait BuildsRandomVectors {
  private val uniform = Uniform(0,1)
  def randomArray(size: Int, offset: Int = 0, stride: Int = 1): DenseVector[Double] = {
    require(offset >= 0)
    require(stride >= 1)
    val v = DenseVector(new Array[Double](offset+stride*size), offset, stride, size)
    val result = DenseVector.zeros[Double](size)
    var i=0;
    while (i < size) {
      result.unsafeUpdate(i, uniform.draw())
      i += 1
    }
    result
  }

  def randomMatrix(m: Int, n: Int): DenseMatrix[Double] = {
    require(m > 0)
    require(n > 0)
    val d = new Array[Double](m*n)
    var i = 0
    while (i < m*n) {
      d(i) = uniform.draw()
      i += 1
    }
    return new DenseMatrix(m, n, d, 0, 1)
  }
}

class DenseVectorBenchmark extends BreezeBenchmark with BuildsRandomVectors {
  def timeAllocate(reps: Int) = run(reps) {
    DenseVector.zeros[Double](1024)
  }
  def timeFill(reps: Int) = run(reps) {
    DenseVector.fill[Double](1024, 23)
  }

  def valueAtBench(reps: Int, size: Int, stride: Int) = runWith(reps, {randomArray(size, stride=stride)})(arr => {
    var i=0;
    var t: Double = 0
    while (i < arr.size) {
      t += arr.valueAt(i) //This is not strictly part of the benchmark, but done so that the JIT doensn't eliminate everything
      i += 1
    }
    t
  })

  def timeValueAt(reps: Int) = valueAtBench(reps, 1024*8, 1)
  def timeValueAtStride4(reps: Int) = valueAtBench(reps, 1024*8, 4)

  def unsafeValueAtBench(reps: Int, size: Int, stride: Int) = runWith(reps, {randomArray(size, stride=stride)})(arr => {
    var i=0;
    var t: Double = 0
    while (i < arr.size) {
      t += arr.unsafeValueAt(i) //This is not strictly part of the benchmark, but done so that the JIT doensn't eliminate everything
      i += 1
    }
    t
  })

  def timeUnsafeValueAt(reps: Int) = unsafeValueAtBench(reps, 1024*8, 1)
  def timeUnsafeValueAtStride4(reps: Int) = unsafeValueAtBench(reps, 1024*8, 4)

  def updateBench(reps: Int, size: Int, stride: Int) = runWith(reps, {randomArray(size, stride=stride)})(arr => {
    var i=0;
    while (i < arr.size) {
      arr.update(i, i.toDouble)
      i += 1
    }
    arr
  })

  def timeUpdate(reps: Int) = updateBench(reps, 1024*8, 1)
  def timeUpdateStride4(reps: Int) = updateBench(reps, 1024*8, 4)

  def unsafeUpdateBench(reps: Int, size: Int, stride: Int) = runWith(reps, {randomArray(size, stride=stride)})(arr => {
    var i=0;
    while (i < arr.size) {
      arr.unsafeUpdate(i, i.toDouble)
      i += 1
    }
    arr
  })

  def timeUnsafeUpdate(reps: Int) = unsafeUpdateBench(reps, 1024*8, 1)
  def timeUnsafeUpdateStride4(reps: Int) = unsafeUpdateBench(reps, 1024*8, 4)

}
