package breeze.signal

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg.DenseVector
import breeze.numerics._

//ToDo: 2D fourierShift/iFourierShift, make horz/vert join function first

/**Inverse shift the zero-frequency component to the center of the spectrum. For odd sequences, this is not
  * equivalent to [[breeze.signal.fourierShift]]
  *
  * @param dft input array
  * @return
  */
object iFourierShift extends UFunc {

  @expand
  @expand.valify
  implicit def implIFourierShift[@expand.args(Int, Long, Float, Double) T]: Impl[DenseVector[T], DenseVector[T]] = {
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(dft: DenseVector[T]): DenseVector[T] = {
        if( isEven(dft.length) ) DenseVector.vertcat( dft( dft.length/2 to -1 ), dft( 0 to dft.length/2 -1 ) )
        else DenseVector.vertcat( dft( (dft.length - 1)/2 to -1 ), dft( 0 to (dft.length - 1)/2 -1) )
      }
    }

  }


}
