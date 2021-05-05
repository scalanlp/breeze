package breeze.linalg

import breeze.benchmark._
import breeze.generic.{MappingUFunc, UFunc}

object MappingUfuncBenchmark extends MyRunner(classOf[MappingUfuncBenchmark])

object addOne extends MappingUFunc {
  //A custom stupid ufunc that is very fast to run
  implicit object expDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = v + 1 }
}

object harderUfunc extends MappingUFunc {
  //A custom stupid ufunc that is very fast to run
  implicit object expDoubleImpl extends Impl[Double, Double] { def apply(v: Double) = /*(v+1)/(1+v*v)*/ math.exp(v) }
}

class MappingUfuncBenchmark extends BreezeBenchmark with BuildsRandomMatrices with BuildsRandomVectors {
  // these should be roughly similar:
  // Group 1
  def timeMappingUfuncDenseMat(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      addOne(mat)
    })

  def timeMappingUfuncDenseVec(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      addOne(arr)
    })

  // Group1 appendix: slower, but that's ok

  def timeMappingUfuncDenseMatWithStride(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048 * 2) })((mat: DenseMatrix[Double]) => {
      val newMat = new DenseMatrix(2048, 2048, mat.data, offset = 0, majorStride = 2048)
      addOne(newMat)
    })

  // Group 2: easy in place

  def timeMappingUfuncDenseVecInPlace(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      addOne.inPlace(arr)
    })

  def timeMappingUfuncArrayInPlace(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      val data = arr.data
      var i = 0
      while (i < data.length) {
        data(i) = addOne(data(i))
        i += 1
      }
      data
    })

  def timeMappingUfuncDenseMatInPlace(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      addOne.inPlace(mat)
    })

  // Group 3: harder

  def timeMappingUfuncArrayHarder(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      val data = arr.data
      val nd = new Array[Double](data.length)
      var i = 0
      while (i < data.length) {
        nd(i) = harderUfunc(data(i))
        i += 1
      }
      nd
    })

  def timeMappingUfuncDenseVecHarder(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      harderUfunc(arr)
    })

  def timeMappingUfuncDenseMatHarder(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      harderUfunc(mat)
    })

  def timeMappingUfuncDenseMatHarderWithStride(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048 * 2) })((mat: DenseMatrix[Double]) => {
      val newMat = new DenseMatrix(2048, 2048, mat.data, offset = 0, majorStride = 2048)
      harderUfunc(newMat)
    })

  def timeMappingUfuncDenseVecHarderMapValues(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      arr.mapValues(harderUfunc(_))
    })

  def timeMappingUfuncDenseMatHarderMapValues(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      mat.mapValues(harderUfunc(_))
    })

  // Group 4: harder inplace

  def timeMappingUfuncArrayHarderInlineInPlace(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      val data = arr.data
      var i = 0
      while (i < data.length) {
        val v = data(i)
        data(i) = math.exp(v) //(v+1)/(1+v*v)
        i += 1
      }
      data
    })

  def timeMappingUfuncDenseMatHarderInPlace(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      harderUfunc.inPlace(mat)
    })

  def timeMappingUfuncDenseVecHarderInPlace(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      harderUfunc.inPlace(arr)
    })

  def timeMappingUfuncArrayHarderInPlace(reps: Int) =
    runWith(reps, { randomArray(2048 * 2048) })((arr: DenseVector[Double]) => {
      val data = arr.data
      var i = 0
      while (i < data.length) {
        data(i) = harderUfunc(data(i))
        i += 1
      }
      data
    })

}
