package breeze

import breeze.macros.{expandArgs, expand}
import breeze.linalg.operators.{OpAdd, BinaryOp}
import breeze.linalg.DenseVector

/**
 * TODO
 *
 * @author dlwh
 **/
class DVVV {
  @expand
  implicit def foo[@expandArgs(Int, Double) T]:BinaryOp[DenseVector[T], DenseVector[T], OpAdd, DenseVector[T]] = new BinaryOp[DenseVector[T], DenseVector[T], OpAdd, DenseVector[T]] {
    def apply(a: DenseVector[T], b: DenseVector[T]): DenseVector[T] = {
      val ad = a.data
      val bd = b.data
      var aoff = a.offset
      var boff = b.offset
      val result = DenseVector.zeros[T](a.length)
      val rd = result.data

      var i = 0
      while(i < a.length) {
        rd(i) = ad(aoff) + bd(boff)
        aoff += a.stride
        boff += b.stride
        i += 1
      }
      result
    }
  }
}
