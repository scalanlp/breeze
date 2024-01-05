package breeze.stats.regression

import breeze.generic.UFunc
import breeze.linalg._
import org.netlib.util.intW
import dev.ludovic.netlib.lapack.LAPACK.{getInstance => lapack}

import java.util.Arrays
import scala.reflect.ClassTag

private object leastSquaresImplementation {
  def doLeastSquares[T: ClassTag](
                         data: DenseMatrix[T],
                         outputs: DenseVector[T],
                         workArray: Array[T])
                                 (implicit num: Numeric[T]): LeastSquaresRegressionResult[T] = {
    import num._
    require(data.rows == outputs.size)
    require(data.rows > data.cols + 1)
    require(workArray.length >= 2 * data.rows * data.cols)

    val info = new intW(0)
    lapack match {
      case lapack_double if num == implicitly[Numeric[Double]] =>
        lapack_double.dgels(
          "N",
          data.rows,
          data.cols,
          1,
          data.data.asInstanceOf[Array[Double]],
          data.rows,
          outputs.data.asInstanceOf[Array[Double]],
          data.rows,
          workArray.asInstanceOf[Array[Double]],
          workArray.length,
          info)

        if (info.`val` < 0) {
          throw new ArithmeticException("Least squares did not converge.")
        }

        val coefficients = new DenseVector[Double](Arrays.copyOf(outputs.data.asInstanceOf[Array[Double]], data.cols))
        var r2 = 0.toDouble
        for (i <- 0 until (data.rows - data.cols)) {
          r2 = r2 + math.pow(outputs.data(data.cols + i).toDouble, num.fromInt(2).toDouble)
        }
        LeastSquaresRegressionResult(coefficients.asInstanceOf[DenseVector[T]], r2.asInstanceOf[T])


      case lapack_float if num == implicitly[Numeric[Float]] =>
        lapack_float.sgels(
          "N",
          data.rows,
          data.cols,
          1,
          data.data.asInstanceOf[Array[Float]],
          data.rows,
          outputs.data.asInstanceOf[Array[Float]],
          data.rows,
          workArray.asInstanceOf[Array[Float]],
          workArray.length,
          info)

        if (info.`val` < 0) {
          throw new ArithmeticException("Least squares did not converge.")
        }

        val coefficients = new DenseVector[Float](Arrays.copyOf(outputs.data.asInstanceOf[Array[Float]], data.cols))
        var r2 = 0.toDouble
        for (i <- 0 until (data.rows - data.cols)) {
          r2 = r2 + math.pow(outputs.data(data.cols + i).toDouble, num.fromInt(2).toDouble)
        }
        LeastSquaresRegressionResult(coefficients.asInstanceOf[DenseVector[T]], r2.toFloat.asInstanceOf[T])
      case _ =>
        throw new UnsupportedOperationException("Unsupported numeric type. Only Float and Double are supported")
    }

  }
}

case class LeastSquaresRegressionResult[T](coefficients: DenseVector[T], rSquared: T)
  extends RegressionResult[DenseVector[T], T] {
  def apply(x: DenseVector[T]): T =

    ( coefficients.asInstanceOf[DenseVector[Double]]  .dot(  x.asInstanceOf[DenseVector[Double]]   )).asInstanceOf[T]

  def apply(X: DenseMatrix[T]): DenseVector[T] =
       ( X .asInstanceOf[DenseMatrix[Double]] *  (coefficients.asInstanceOf[DenseVector[Double]])).asInstanceOf[DenseVector[T]]
}


object leastSquares extends UFunc {
  implicit def matrixVectorWithWorkArray[T: ClassTag](
                         implicit num: Numeric[T]): Impl3[DenseMatrix[T], DenseVector[T], Array[T], LeastSquaresRegressionResult[T]] =
    new Impl3[DenseMatrix[T], DenseVector[T], Array[T], LeastSquaresRegressionResult[T]] {
      def apply(
                 data: DenseMatrix[T],
                 outputs: DenseVector[T],
                 workArray: Array[T]): LeastSquaresRegressionResult[T] =
        leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, workArray)
    }

  implicit def matrixVectorSpecifiedWork[T: ClassTag](
                          implicit num: Numeric[T]): Impl3[DenseMatrix[T], DenseVector[T], Int, LeastSquaresRegressionResult[T]] =
    new Impl3[DenseMatrix[T], DenseVector[T], Int, LeastSquaresRegressionResult[T]] {
      def apply(
                 data: DenseMatrix[T],
                 outputs: DenseVector[T],
                 workSize: Int): LeastSquaresRegressionResult[T] =
        leastSquaresImplementation.doLeastSquares(data.copy, outputs.copy, new Array[T](workSize))
    }

  implicit def matrixVector[T: ClassTag](
                          implicit num: Numeric[T]): Impl2[DenseMatrix[T], DenseVector[T], LeastSquaresRegressionResult[T]]  =
    new Impl2[DenseMatrix[T], DenseVector[T], LeastSquaresRegressionResult[T]] {
      def apply(
                 data: DenseMatrix[T],
                 outputs: DenseVector[T]): LeastSquaresRegressionResult[T] =
        leastSquaresImplementation.doLeastSquares(
          data.copy,
          outputs.copy,
          new Array[T](math.max(1, data.rows * data.cols * 2)))
    }
}


object leastSquaresDestructive extends UFunc {

  implicit def matrixVectorWithWorkArray[T: ClassTag](
                           implicit num: Numeric[T]): Impl3[DenseMatrix[T], DenseVector[T], Array[T], LeastSquaresRegressionResult[T]] =
    new Impl3[DenseMatrix[T], DenseVector[T], Array[T], LeastSquaresRegressionResult[T]] {
      def apply(
                 data: DenseMatrix[T],
                 outputs: DenseVector[T],
                 workArray: Array[T]): LeastSquaresRegressionResult[T] =
        leastSquaresImplementation.doLeastSquares(data, outputs, workArray)
    }

  implicit def matrixVectorSpecifiedWork[T: ClassTag](
                        implicit num: Numeric[T]): Impl3[DenseMatrix[T], DenseVector[T], Int, LeastSquaresRegressionResult[T]] =
    new Impl3[DenseMatrix[T], DenseVector[T], Int, LeastSquaresRegressionResult[T]] {
      def apply(
                 data: DenseMatrix[T],
                 outputs: DenseVector[T],
                 workSize: Int): LeastSquaresRegressionResult[T] =
        leastSquaresImplementation.doLeastSquares(data, outputs, new Array[T](workSize))
    }

  implicit def matrixVector[T: ClassTag](
                        implicit num: Numeric[T]): Impl2[DenseMatrix[T], DenseVector[T], LeastSquaresRegressionResult[T]] =
    new Impl2[DenseMatrix[T], DenseVector[T], LeastSquaresRegressionResult[T]] {
      def apply(
                 data: DenseMatrix[T],
                 outputs: DenseVector[T]): LeastSquaresRegressionResult[T] =
        leastSquaresImplementation.doLeastSquares(
          data,
          outputs,
          new Array[T](math.max(1, data.rows * data.cols * 2)))
    }

}
