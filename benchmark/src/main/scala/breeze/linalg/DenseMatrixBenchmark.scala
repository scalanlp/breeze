package breeze.linalg

import breeze.benchmark._

import breeze.linalg._
import breeze.stats.distributions._
import spire.implicits._

object DenseMatrixBenchmark extends MyRunner(classOf[DenseMatrixBenchmark])

trait BuildsRandomMatrices {
  private val uniform = Uniform(0,1)
  def randomMatrix(m: Int, n: Int): DenseMatrix[Double] = { DenseMatrix.rand[Double](m,n) }
}

class DenseMatrixBenchmark extends BreezeBenchmark with BuildsRandomMatrices {
  def timeValueAt(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    var t: Double = 0
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        t += mat(i,j)
      })
    })
    t
  })
  def timeUnsafeValueAt(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    var t: Double = 0
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        t += mat.unsafeValueAt(i,j)
      })
    })
    t
  })
  def timeUpdate(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        mat.update(i,j,i+j)
      })
    })
    mat
  })
  def timeUnsafeUpdate(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        mat.unsafeUpdate(i,j,i+j)
      })
    })
    mat
  })
}
