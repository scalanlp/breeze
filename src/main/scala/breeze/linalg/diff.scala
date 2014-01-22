package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand

/**
 * Differences between adjacent elements and discrete derivatives.
 */
object diff extends UFunc {
  @expand
  @expand.valify
  implicit def implDV_Double_DV[@expand.args(Int, Long, Float, Double) T]: Impl2[DenseVector[T], Int, DenseVector[T]] = {
    new Impl2[DenseVector[T], Int, DenseVector[T]] {
      def apply(v: DenseVector[T], n: Int): DenseVector[T] = {
        if(n <= 0) v else this.apply( diff(v), n-1)
      }
    }

  }

  @expand
  @expand.valify
  implicit def implDV_DV[@expand.args(Int, Long, Float, Double) T]: Impl[DenseVector[T], DenseVector[T]] = {
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(v: DenseVector[T]): DenseVector[T] = {
        if(v.length <= 1) DenseVector[T]()
        else DenseVector.tabulate(v.length - 1)( index =>  v(index+1) - v(index) )
      }
    }

  }

}
