package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand

/**
 * Differences between adjacent elements and discrete derivatives.
 */
object diff extends UFunc {
//  @expand
//  @expand.valify

  implicit object implDV_Double_DV extends Impl2[DenseVector[/*@expand.args(Int, Long, Float, Double)*/ Double], Int, DenseVector[Double]] {

    def apply(v: DenseVector[Double], n: Int): DenseVector[Double] = {
      if(n <= 0) v else this.apply( implDV_DV(v), n-1)
    }

  }

  implicit object implDV_DV extends Impl[DenseVector[Double], DenseVector[Double]] {

    def apply(v: DenseVector[Double]): DenseVector[Double] = {
      if(v.length <= 1) DenseVector[Double]()
      else DenseVector.tabulate(v.length - 1)( index =>  v(index+1) - v(index) )
    }

  }

}
