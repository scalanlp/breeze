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
  def timeValueAtRowCol(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    var t: Double = 0
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        t += mat(i,j)
      })
    })
    t
  })
  def timeUnsafeValueAtRowCol(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    var t: Double = 0
    cfor(0)(j=>j<size, j=>j+1)(j => {
      cfor(0)(i=>i<size, i=>i+1)(i => {
        t += mat.unsafeValueAt(i,j)
      })
    })
    t
  })
  def timeValueAtColRow(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    var t: Double = 0
    cfor(0)(j=>j<size, j=>j+1)(j => {
      cfor(0)(i=>i<size, i=>i+1)(i => {
        t += mat(i,j)
      })
    })
    t
  })
  def timeUnsafeValueAtColRow(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    var t: Double = 0
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        t += mat.unsafeValueAt(i,j)
      })
    })
    t
  })

  def timeUpdateRowCol(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        mat.update(i,j,i+j)
      })
    })
    mat
  })
  def timeUnsafeUpdateRowCol(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    cfor(0)(i=>i<size, i=>i+1)(i => {
      cfor(0)(j=>j<size, j=>j+1)(j => {
        mat.unsafeUpdate(i,j,i+j)
      })
    })
    mat
  })
  def timeUpdateColRow(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    cfor(0)(j=>j<size, j=>j+1)(j => {
      cfor(0)(i=>i<size, i=>i+1)(i => {
        mat.update(i,j,i+j)
      })
    })
    mat
  })
  def timeUnsafeUpdateColRow(reps: Int) = runWith(reps, {randomMatrix(2048,2048)})((mat:DenseMatrix[Double]) => {
    val size = 2048
    cfor(0)(j=>j<size, j=>j+1)(j => {
      cfor(0)(i=>i<size, i=>i+1)(i => {
        mat.unsafeUpdate(i,j,i+j)
      })
    })
    mat
  })
}
