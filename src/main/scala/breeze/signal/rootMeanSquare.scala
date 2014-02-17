package breeze.signal

import breeze.generic.UFunc
import breeze.linalg.{mean, DenseMatrix, DenseVector}
import breeze.numerics._
import breeze.macros.expand

/**Root mean square of a vector.
 * @author ktakagaki
 * @date 2/17/14.
 */
object rootMeanSquare extends UFunc {

  /** Use via implicit delegate syntax rootMeanSquare(x: DenseVector)
    *
    */
  @expand.valify
  @expand
  implicit def rms1D[@expand.args(Int, Long, Float, Double) T] : rootMeanSquare.Impl[DenseVector[T], Double] = {
    new rootMeanSquare.Impl[DenseVector[T], Double] {
      def apply(v: DenseVector[T]): Double = {
        val temp: Double = mean( v.map( (elem: T) => elem * elem )  )
        sqrt( temp )
      }
    }
  }


}