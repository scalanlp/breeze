package breeze.linalg

import breeze.benchmark._
import breeze.generic.{MappingUFunc, UFunc}

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

  def timeMappingUfuncDenseMatInPlace(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    addOne.inPlace(mat)
  })

  def timeMappingUfuncDenseMatWithStride(reps: Int) = runWith(reps, {randomMatrix(2048,2048*2)})((mat:DenseMatrix[Double]) => {
    val newMat = new DenseMatrix(2048, 2048, mat.data, offset=0, majorStride=2048)
    addOne(newMat)
  })

  def timeMappingUfuncDenseMatHarder(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    harderUfunc(mat)
  })

  def timeMappingUfuncDenseMatHarderInPlace(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    harderUfunc.inPlace(mat)
  })

  def timeMappingUfuncDenseMatHarderWithStride(reps: Int) = runWith(reps, {randomMatrix(2048,2048*2)})((mat:DenseMatrix[Double]) => {
    val newMat = new DenseMatrix(2048, 2048, mat.data, offset=0, majorStride=2048)
    harderUfunc(newMat)
  })

  def timeMappingUfuncDenseVec(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    addOne(arr)
  })

  def timeMappingUfuncDenseVecHarder(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    harderUfunc(arr)
  })

  def timeMappingUfuncDenseVecInPlace(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    addOne.inPlace(arr)
  })

  def timeMappingUfuncDenseVecHarderInPlace(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    harderUfunc.inPlace(arr)
  })

  def timeMappingUfuncArrayInPlace(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    val data = arr.data
    var i=0
    while (i < data.length) {
      data(i) = addOne(data(i))
      i += 1
    }
    data
  })

  def timeMappingUfuncArrayHarderInPlace(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    val data = arr.data
    var i=0
    while (i < data.length) {
      data(i) = harderUfunc(data(i))
      i += 1
    }
    data
  })

  def timeMappingUfuncArrayHarder(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    val data = arr.data
    val nd = new Array[Double](data.length)
    var i=0
    while (i < data.length) {
      nd(i) = harderUfunc(data(i))
      i += 1
    }
    nd
  })

  def timeMappingUfuncArrayHarderInlineInPlace(reps: Int) = runWith(reps, {randomArray(2048*2048)})((arr:DenseVector[Double]) => {
    val data = arr.data
    var i=0
    while (i < data.length) {
      val v = data(i)
      data(i) = (v+1)/(1+v*v)
      i += 1
    }
    data
  })

}
