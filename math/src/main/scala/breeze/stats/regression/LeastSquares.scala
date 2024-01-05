package breeze.stats.regression

import breeze.generic.UFunc
import breeze.linalg._
import org.netlib.util.intW
import dev.ludovic.netlib.lapack.LAPACK.{getInstance => lapack}
import java.util.Arrays

private object leastSquaresImplementation {
  def doLeastSquares(
      data: DenseMatrix[Double],
      outputs: DenseVector[Double],
      workArray: Array[Double]): LeastSquaresRegressionResult = {
    require(data.rows == outputs.size)
    require(data.rows > data.cols + 1)
    require(workArray.length >= 2 * data.rows * data.cols)

    val info = new intW(0)
    lapack.dgels(
      "N",
      data.rows,
      data.cols,
      1,
      data.data,
      data.rows,
      outputs.data,
      data.rows,
      workArray,
      workArray.length,
      info)
    if (info.`val` < 0) {
      throw new ArithmeticException("Least squares did not converge.")
    }

    val coefficients = new DenseVector[Double](Arrays.copyOf(outputs.data, data.cols))
    var r2 = 0.0
    for (i <- 0 until (data.rows - data.cols)) {
      r2 = r2 + math.pow(outputs.data(data.cols + i), 2)
    }
    LeastSquaresRegressionResult(coefficients, r2)
  }
}

case class LeastSquaresRegressionResult(coefficients: DenseVector[Double], rSquared: Double)
    extends RegressionResult[DenseVector[Double], Double] {
  def apply(x: DenseVector[Double]): Double = coefficients.dot(x)

  def apply(X: DenseMatrix[Double]): DenseVector[Double] = X * coefficients
}

object leastSquares extends UFunc {
  implicit val matrixVectorWithWorkArrayDouble
    : Impl3[DenseMatrix[Double], DenseVector[Double], Array[Double], LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Double], DenseVector[Double], Array[Double], LeastSquaresRegressionResult] {
      def apply(
          data: DenseMatrix[Double],
          outputs: DenseVector[Double],
          workArray: Array[Double]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, workArray)
    }

  implicit val matrixVectorWithWorkArrayFloat
  : Impl3[DenseMatrix[Float], DenseVector[Float], Array[Float], LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Float], DenseVector[Float], Array[Float], LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Float],
                 outputs: DenseVector[Float],
                 workArray: Array[Float]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, workArray)
    }

  implicit val matrixVectorSpecifiedWorkDouble
    : Impl3[DenseMatrix[Double], DenseVector[Double], Int, LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Double], DenseVector[Double], Int, LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Double],
                 outputs: DenseVector[Double],
                 workSize: Int): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, new Array[Double](workSize))
    }

  implicit val matrixVectorSpecifiedWorkFloat
  : Impl3[DenseMatrix[Float], DenseVector[Float], Int, LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Float], DenseVector[Float], Int, LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Float],
                 outputs: DenseVector[Float],
                 workSize: Int): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, new Array[Float](workSize))
    }

  implicit val matrixVectorDouble: Impl2[DenseMatrix[Double], DenseVector[Double], LeastSquaresRegressionResult] =
    new Impl2[DenseMatrix[Double], DenseVector[Double], LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Double],
                 outputs: DenseVector[Double]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(
          data.copy,
          outputs.copy,
          new Array[Double](math.max(1, data.rows * data.cols * 2)))
    }

  implicit val matrixVectorFloat: Impl2[DenseMatrix[Float], DenseVector[Float], LeastSquaresRegressionResult] =
    new Impl2[DenseMatrix[Float], DenseVector[Float], LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Float],
                 outputs: DenseVector[Float]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(
          data.copy,
          outputs.copy,
          new Array[Float](math.max(1, data.rows * data.cols * 2)))
    }
}

object leastSquaresDestructive extends UFunc {
  implicit val matrixVectorWithWorkArrayDouble
    : Impl3[DenseMatrix[Double], DenseVector[Double], Array[Double], LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Double], DenseVector[Double], Array[Double], LeastSquaresRegressionResult] {
      def apply(
          data: DenseMatrix[Double],
          outputs: DenseVector[Double],
          workArray: Array[Double]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data, outputs, workArray)
    }

  implicit val matrixVectorWithWorkArrayFloat
  : Impl3[DenseMatrix[Float], DenseVector[Float], Array[Float], LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Float], DenseVector[Float], Array[Float], LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Float],
                 outputs: DenseVector[Float],
                 workArray: Array[Float]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data, outputs, workArray)
    }

  implicit val matrixVectorSpecifiedWorkDouble
    : Impl3[DenseMatrix[Double], DenseVector[Double], Int, LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Double], DenseVector[Double], Int, LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Double],
                 outputs: DenseVector[Double],
                 workSize: Int): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data, outputs, new Array[Float](workSize))
    }

  implicit val matrixVectorSpecifiedWorkFloat
  : Impl3[DenseMatrix[Float], DenseVector[Float], Int, LeastSquaresRegressionResult] =
    new Impl3[DenseMatrix[Float], DenseVector[Float], Int, LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Float],
                outputs: DenseVector[Float],
                workSize: Int): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(data, outputs, new Array[Float](workSize))
    }

  implicit val matrixVectorDouble: Impl2[DenseMatrix[Double], DenseVector[Double], LeastSquaresRegressionResult] =
    new Impl2[DenseMatrix[Double], DenseVector[Double], LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Double],
                outputs: DenseVector[Double]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(
          data,
          outputs,
          new Array[Double](math.max(1, data.rows * data.cols * 2)))
    }

  implicit val matrixVectorFloat: Impl2[DenseMatrix[Float], DenseVector[Float], LeastSquaresRegressionResult] =
    new Impl2[DenseMatrix[Float], DenseVector[Float], LeastSquaresRegressionResult] {
      def apply(
                 data: DenseMatrix[Float],
                 outputs: DenseVector[Float]): LeastSquaresRegressionResult =
        leastSquaresImplementation.doLeastSquares(
          data,
          outputs,
          new Array[Float](math.max(1, data.rows * data.cols * 2)))
    }
}
