package breeze

import breeze.macros.{sequence, expandArgs, expand}
import breeze.linalg.operators._
import breeze.linalg.DenseVector

/**
 * TODO
 *
 * @author dlwh
 **/
class DVVV {
  @expand
  implicit def dvOp[@expandArgs(Int, Double, Float, Long) T,
                   @expandArgs(OpAdd, OpSub, OpMulScalar, OpDiv) Op <: OpType]
                   (implicit @sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _})
                   op: BinaryOp[T, T, Op, T]):BinaryOp[DenseVector[T], DenseVector[T], Op, DenseVector[T]] = new BinaryOp[DenseVector[T], DenseVector[T], Op, DenseVector[T]] {
    def apply(a: DenseVector[T], b: DenseVector[T]): DenseVector[T] = {
      val ad = a.data
      val bd = b.data
      var aoff = a.offset
      var boff = b.offset
      val result = DenseVector.zeros[T](a.length)
      val rd = result.data


      var i = 0
      while(i < a.length) {
        rd(i) = op(ad(aoff), bd(boff))
        aoff += a.stride
        boff += b.stride
        i += 1
      }
      result
    }
  }
}
