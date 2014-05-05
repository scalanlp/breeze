package breeze.linalg

import breeze.generic.{UFunc, MappingUFunc}
import breeze.macros.expand

/**
 * Provides casting facilities similar to Numpy's "astype" and Julia's "convert". Does casts of
 * collections from one type to another. For example,
 *
 * {{{
 * convert(DenseVector(0.1, 1.0, 1.5), Int) == DenseVector(0, 1, 1)
 * }}}
 * @author dlwh
 */
object convert extends UFunc with MappingUFunc {
  @expand
  @expand.valify
  implicit def convInts[@expand.args(Int, Double, Float, Long, Char, Short) From,
                        @expand.args(Int, Double, Float, Long, Char, Short) To]
  (implicit @expand.sequence[To](_.toInt, _.toDouble, _.toFloat, _.toLong, _.toChar, _.toShort) conv: From=>To): Impl2[From, To.type, To] =  {
    new Impl2[From, To.type, To] {
      def apply(v: From, v2: To.type): To = conv(v)
    }
  }


}
