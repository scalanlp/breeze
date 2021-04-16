package breeze.linalg

import breeze.benchmark._
import breeze.numerics.sin
import breeze.stats.distributions._
import breeze.macros._

object DenseMatrixBenchmark extends MyRunner(classOf[DenseMatrixBenchmark])

trait BuildsRandomMatrices {
  private val uniform = Uniform(0, 1)
  def randomMatrix(m: Int, n: Int, transpose: Boolean = false): DenseMatrix[Double] = {
    if (!transpose) {
      DenseMatrix.rand[Double](m, n)
    } else {
      DenseMatrix.rand[Double](m, n).t
    }
  }

  def randomIntMatrix(m: Int, n: Int, transpose: Boolean = false): DenseMatrix[Int] = {
    if (!transpose) {
      DenseMatrix.rand[Int](m, n, rand = Rand.randInt(200))
    } else {
      DenseMatrix.rand[Int](m, n, Rand.randInt(200)).t
    }
  }
}

class DenseMatrixBenchmark extends BreezeBenchmark with BuildsRandomMatrices {
  def timeValueAtRowCol(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      val size = 2048
      var t: Double = 0
      cforRange(0 until size)(i => {
        cforRange(0 until size)(j => {
          t += mat(i, j)
        })
      })
      t
    })

  def timeUnsafeValueAtRowCol(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      val size = 2048
      var t: Double = 0
      cforRange(0 until size)(i => {
        cforRange(0 until size)(j => {
          t += mat.unsafeValueAt(i, j)
        })
      })
      t
    })

  def timeValueAtColRow(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      val size = 2048
      var t: Double = 0
      cforRange(0 until size)(j => {
        cforRange(0 until size)(i => {
          t += mat(i, j)
        })
      })
      t
    })

  def timeUnsafeValueAtColRow(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      val size = 2048
      var t: Double = 0
      cforRange(0 until size)(j => {
        cforRange(0 until size)(i => {
          t += mat.unsafeValueAt(i, j)
        })
      })
      t
    })

  def timeUpdateRowCol(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      val size = 2048
      cforRange(0 until size)(i => {
        cforRange(0 until size)(j => {
          mat.update(i, j, i + j)
        })
      })
      mat
    })
  def timeUnsafeUpdateRowCol(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      val size = 2048
      cforRange(0 until size)(i => {
        cforRange(0 until size)(j => {
          mat.unsafeUpdate(i, j, i + j)
        })
      })
      mat
    })
  def timeUpdateColRow(reps: Int) =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      val size = 2048
      cforRange(0 until size)(j => {
        cforRange(0 until size)(i => {
          mat.update(i, j, i + j)
        })
      })
      mat
    })

  def timeMapPairs(reps: Int): DenseMatrix[Double] =
    runWith(reps, { randomMatrix(2048, 2048) })((mat: DenseMatrix[Double]) => {
      mat.mapPairs((x: (Int, Int), v: Double) => (x._1 * x._2 * v))
    })
  def timeMapPairsTranspose(reps: Int): DenseMatrix[Double] =
    runWith(reps, { randomMatrix(2048, 2048, true) })((mat: DenseMatrix[Double]) => {
      mat.mapPairs((x: (Int, Int), v: Double) => (x._1 * x._2 * v))
    })

  def timeSinMatrix(reps: Int): DenseMatrix[Double] = runWith(reps, randomMatrix(2500, 2500)) { dm =>
    sin(dm)
  }

//  def timeIntMatrixMultiply(reps: Int) = runWith(reps, randomIntMatrix(2500, 2500)): Unit = { dm =>
//    dm * dm
//  }
}
