package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand

/**
* Returns a cumulative sum of the vector (ie cumsum).
* @author ktakagaki
*/
object accumulate extends UFunc {

  @expand
  @expand.valify
  implicit def dvAccumulate[@expand.args(Int, Long, Float, Double) T]: Impl[DenseVector[T], DenseVector[T]] =
    new Impl[DenseVector[T], DenseVector[T]] {
      def apply(dv: DenseVector[T]): DenseVector[T] = {
        DenseVector( cumImpl(List[T]( 0.asInstanceOf[T] ), dv.toArray.toList).reverse.drop(1).toArray )
      }
      def cumImpl(listNew: List[T], listOld: List[T]): List[T] = {
        if(listOld == Nil) listNew
        else cumImpl( (listOld.head + listNew.head) :: listNew, listOld.tail)
      }
    }

}
