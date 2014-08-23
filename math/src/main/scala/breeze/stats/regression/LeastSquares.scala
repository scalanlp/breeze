package breeze.stats.regression

import breeze.generic.UFunc
import breeze.linalg._
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import java.util.Arrays

private object leastSquaresImplementation {
  def doLeastSquares(data: DenseMatrix[Double], outputs: DenseVector[Double], workArray: Array[Double]): LeastSquaresRegressionResult = {
    require(data.rows == outputs.size)
    require(data.rows > data.cols)
    require(data.rows == outputs.size)
    require(workArray.size >= 2*data.rows*data.cols)

    val info = new intW(0)
    lapack.dgels("N", data.rows, data.cols, 1, data.data, data.rows, outputs.data, data.rows, workArray, workArray.size, info)
    if (info.`val` < 0) {
      throw new ArithmeticException("Least squares did not converge.")
    }

    val resultVec = new DenseVector[Double](Arrays.copyOf(outputs.data, data.cols))
    LeastSquaresRegressionResult(resultVec, outputs.data(data.cols+1))
  }
}

case class LeastSquaresRegressionResult(coefficients: DenseVector[Double], rSquared: Double) extends RegressionResult[DenseVector[Double], Double] {
  def apply(x: DenseVector[Double]): Double = coefficients.dot(x)
}

object leastSquares extends UFunc {
  implicit val matrixVectorWithWorkArray = new Impl3[DenseMatrix[Double], DenseVector[Double], Array[Double], LeastSquaresRegressionResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double], workArray: Array[Double]): LeastSquaresRegressionResult = leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, workArray)
  }

  implicit val matrixVectorSpecifiedWork = new Impl3[DenseMatrix[Double], DenseVector[Double], Int, LeastSquaresRegressionResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double], workSize: Int): LeastSquaresRegressionResult = leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, new Array[Double](workSize))
  }

  implicit val matrixVector = new Impl2[DenseMatrix[Double], DenseVector[Double], LeastSquaresRegressionResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double]): LeastSquaresRegressionResult = leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, new Array[Double](math.max(1, data.rows*data.cols*2)))
  }
}

object leastSquaresDestructive extends UFunc {
  implicit val matrixVectorWithWorkArray = new Impl3[DenseMatrix[Double], DenseVector[Double], Array[Double], LeastSquaresRegressionResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double], workArray: Array[Double]): LeastSquaresRegressionResult = leastSquaresImplementation.doLeastSquares(data, outputs, workArray)
  }

  implicit val matrixVectorSpecifiedWork = new Impl3[DenseMatrix[Double], DenseVector[Double], Int, LeastSquaresRegressionResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double], workSize: Int): LeastSquaresRegressionResult = leastSquaresImplementation.doLeastSquares(data, outputs, new Array[Double](workSize))
  }

  implicit val matrixVector = new Impl2[DenseMatrix[Double], DenseVector[Double], LeastSquaresRegressionResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double]): LeastSquaresRegressionResult = leastSquaresImplementation.doLeastSquares(data, outputs, new Array[Double](math.max(1, data.rows*data.cols*2)))
  }
}
