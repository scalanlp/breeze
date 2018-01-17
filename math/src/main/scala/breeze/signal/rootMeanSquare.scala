package breeze.signal

import breeze.generic.UFunc
import breeze.linalg._
import breeze.numerics._
import breeze.macros.expand
import breeze.math.Complex
import breeze.stats.mean

/**Root mean square of a vector.
 * @author ktakagaki
 * @date 2/17/14.
 */
object rootMeanSquare extends UFunc {

  //ToDo: Expand to Int and Long once mean() is generified

  @expand
  @expand.valify
  implicit def rms1D[@expand.args(Float, Double) T]: rootMeanSquare.Impl[DenseVector[T], T] = {
    new rootMeanSquare.Impl[DenseVector[T], T] {
      def apply(v: DenseVector[T]): T = {
        val temp: T = mean(v.map((elem: T) => elem * elem))
        sqrt(temp)
      }
    }
  }

  //temporary placeholder implementation until mean is fully generified
  implicit def rms1DInt: rootMeanSquare.Impl[DenseVector[Int], Double] = {
    new rootMeanSquare.Impl[DenseVector[Int], Double] {
      def apply(v: DenseVector[Int]): Double =
        sqrt(sum(convert(v, Double).map(elem => elem * elem)) / v.length.toDouble)
    }
  }
  implicit def rms1DLong: rootMeanSquare.Impl[DenseVector[Long], Double] = {
    new rootMeanSquare.Impl[DenseVector[Long], Double] {
      def apply(v: DenseVector[Long]): Double =
        sqrt(sum(convert(v, Double).map(elem => elem * elem)) / v.length.toDouble)
    }
  }

}
