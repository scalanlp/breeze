package breeze.stats.regression

import breeze.generic.UFunc
import breeze.linalg._
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import java.util.Arrays

object leastSquares extends UFunc {
  case class LeastSquaresRegressionResult(coefficients: DenseVector[Double], rSquared: Double) extends RegressionResult[DenseVector[Double], Double] {
    def apply(x: DenseVector[Double]): Double = coefficients.dot(x)
  }

  implicit val matrixVector = new Impl2[DenseMatrix[Double], DenseVector[Double], LeastSquaresRegressionResult] {
    def apply(data: DenseMatrix[Double], outputs: DenseVector[Double]): LeastSquaresRegressionResult = {
      require(data.rows == outputs.size)
      require(data.rows > data.cols)
      require(data.rows == outputs.size)

      val d = data.copy
      val result = outputs.copy
      val workArray = new Array[Double](math.max(1, data.rows*data.cols*2))

      val info = new intW(0)
      lapack.dgels("N", data.rows, data.cols, 1, d.data, data.rows, result.data, data.rows, workArray, workArray.size, info)
      if (info.`val` < 0) {
        throw new ArithmeticException("Least squares did not converge.")
      }

      val resultVec = new DenseVector[Double](Arrays.copyOf(result.data, data.cols))
      LeastSquaresRegressionResult(resultVec, result.data(data.cols+1))
    }
  }
}
