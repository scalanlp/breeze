package breeze.stats.regression

import breeze.generic.UFunc
import breeze.linalg._
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import java.util.Arrays
import spire.implicits.cfor

private case class LassoCalculator(data: DenseMatrix[Double], outputs: DenseVector[Double], lambda: Double, workArray: Array[Double], MAX_ITER: Int=100, IMPROVE_THRESHOLD: Double=1e-8) {
  /*
   * The main purpose of this complicated calculator object is to recycle all the assorted work arrays.
   * If we didn't write it this way, we'd have to manually thread all the work arrays
   * throughout a slew of functions.
   */
  require(data.rows == outputs.size)
  require(data.rows > data.cols)
  require(data.rows == outputs.size)
  require(workArray.size >= 2*data.rows*data.cols)

  private val outputCopy = new DenseVector[Double](new Array[Double](outputs.size))
  private val singleColumnMatrix = new DenseMatrix[Double](data.rows, 1)
  private val resultVec = new DenseVector[Double](new Array[Double](data.cols))

  lazy val result: LassoResult = {

    var improvedResult = true
    var iter = 0

    while (improvedResult && (iter < MAX_ITER)) {
      iter += 1
      improvedResult = false
      cfor(0)(i => i<data.cols, i=>i+1)(i => {
        val eoc = estimateOneColumn(i)
        val oldCoefficient = resultVec.unsafeValueAt(i)
        resultVec.unsafeUpdate(i, shrink(eoc.coefficients(0)))
        if (oldCoefficient != resultVec.unsafeValueAt(i)) {
          improvedResult = true
        }
      })
    }

    LassoResult(resultVec, 0.0, lambda)
  }

  private def shrink(x: Double): Double = {
    val sb = math.signum(x)
    val ab = sb*x
    if (ab > lambda) {
      sb*(ab-lambda)
    } else {
      0.0
    }
  }

  private def copyColumn(column: Int): Unit = {
    require(column < data.cols)
    require(column >= 0)
    cfor(0)(i => i < outputs.size, i => i+1)(i => {
      singleColumnMatrix.unsafeUpdate(i, 0, data.unsafeValueAt(i, column))
      var o = outputs.unsafeValueAt(i)
      cfor(0)(j => j < data.cols, j => j+1)(j => {
        if (j != column) {
          o -= data.unsafeValueAt(i,j) * resultVec.unsafeValueAt(j)
        }
      })
      outputCopy.unsafeUpdate(i, o)
    })
  }

  private def estimateOneColumn(column: Int): LeastSquaresRegressionResult = {
    copyColumn(column)
    leastSquaresDestructive(singleColumnMatrix, outputCopy, workArray)
  }
}

case class LassoResult(coefficients: DenseVector[Double], rSquared: Double, lambda: Double) extends RegressionResult[DenseVector[Double], Double] {
  require(lambda >= 0)
  def apply(x: DenseVector[Double]): Double = coefficients.dot(x)
}

object lasso extends UFunc {
  implicit val matrixVectorWithWorkArray: Impl4[DenseMatrix[Double], DenseVector[Double], Double, Array[Double], LassoResult] = new Impl4[DenseMatrix[Double], DenseVector[Double], Double, Array[Double], LassoResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double], lambda: Double, workArray: Array[Double]): LassoResult = LassoCalculator(data, outputs, lambda, workArray).result
  }

  implicit val matrixVectorSpecifiedWork: Impl4[DenseMatrix[Double], DenseVector[Double], Double, Int, LassoResult] = new Impl4[DenseMatrix[Double], DenseVector[Double], Double, Int, LassoResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double], lambda: Double, workSize: Int): LassoResult = LassoCalculator(data, outputs, lambda, new Array[Double](workSize)).result
  }

  implicit val matrixVector: Impl3[DenseMatrix[Double], DenseVector[Double], Double, LassoResult] = new Impl3[DenseMatrix[Double], DenseVector[Double], Double, LassoResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double], lambda: Double): LassoResult = LassoCalculator(data.copy, outputs.copy, lambda, new Array[Double](math.max(1, data.rows*data.cols*2))).result
  }
}
