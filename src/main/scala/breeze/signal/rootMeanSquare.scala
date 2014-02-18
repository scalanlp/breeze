package breeze.signal

import breeze.generic.UFunc
import breeze.linalg.{mean, DenseMatrix, DenseVector}
import breeze.numerics._
import breeze.macros.expand
import breeze.math.Complex

/**Root mean square of a vector.
 * @author ktakagaki
 * @date 2/17/14.
 */
object rootMeanSquare extends UFunc {

  //ToDo: Expand to Int and Long once mean() is generified

  /** Use via implicit delegate syntax rootMeanSquare(x: DenseVector)
    *
    */
  @expand
  @expand.valify
  implicit def rms1D[@expand.args(Float, Double) T] : rootMeanSquare.Impl[DenseVector[T], T] = {
    new rootMeanSquare.Impl[DenseVector[T], T] {
      def apply(v: DenseVector[T]): T = {
        val temp: T = mean( v.map( (elem: T) => elem * elem )  )
        sqrt( temp )
      }
    }
  }


}