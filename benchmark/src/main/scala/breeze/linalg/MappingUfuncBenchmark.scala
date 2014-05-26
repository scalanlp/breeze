package breeze.linalg

import breeze.benchmark._
import breeze.generic.{MappingUFunc, UFunc}

import breeze.linalg._
import breeze.stats.distributions._
import breeze.numerics._
import spire.implicits._

object MappingUfuncBenchmark extends MyRunner(classOf[MappingUfuncBenchmark])

object addOne extends UFunc with MappingUFunc {
  //A custom stupid ufunc that is very fast to run
  implicit object expDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = v+1 }
}

object harderUfunc extends UFunc with MappingUFunc {
  //A custom stupid ufunc that is very fast to run
  implicit object expDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = (v+1)/(1+v*v) }
}

class MappingUfuncBenchmark extends BreezeBenchmark with BuildsRandomMatrices with BuildsRandomVectors {
  def timeMappingUfuncDenseMat(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    addOne(mat)
  })

  def timeMappingUfuncDenseMatHarder(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    harderUfunc(mat)
  })

  def timeMappingUfuncDenseVec(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    addOne(arr)
  })

  def timeMappingUfuncDenseVecHarder(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    harderUfunc(arr)
  })

  def timeMappingUfuncArray(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    val data = arr.data
    var i=0
    while (i < data.size) {
      data(i) = addOne(data(i))
      i += 1
    }
    data
  })

  def timeMappingUfuncArrayHarder(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    val data = arr.data
    var i=0
    while (i < data.size) {
      data(i) = harderUfunc(data(i))
      i += 1
    }
    data
  })

}
