package breeze.linalg

import breeze.benchmark._

import breeze.linalg._
import breeze.stats.distributions._
import breeze.numerics._
import spire.implicits._

object MappingUfuncBenchmark extends MyRunner(classOf[MappingUfuncBenchmark])

class MappingUfuncBenchmark extends BreezeBenchmark with BuildsRandomMatrices with BuildsRandomVectors {
  def timeMappingUfuncDenseMat(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    exp(mat)
  })

  def timeMappingUfuncDenseVec(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    exp(arr)
  })

  def timeMappingUfuncArray(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    val data = arr.data
    var i=0
    while (i < data.size) {
      data(i) = exp(data(i))
      i += 1
    }
    data
  })

}
