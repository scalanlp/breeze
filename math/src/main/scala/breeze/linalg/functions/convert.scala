package breeze.linalg

import breeze.generic.{UFunc, MappingUFunc}
import breeze.macros.expand
import breeze.math.Complex

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
  implicit def impl2[
      @expand.args(Int, Double, Float, Long, Char, Short) From,
      @expand.args(Int, Double, Float, Long, Char, Short, Complex) To
  ](implicit
      @expand.sequence[To](
        _.toInt,
        _.toDouble,
        _.toFloat,
        _.toLong,
        _.toChar,
        _.toShort,
        Complex(_, 0)
      ) conv: From => To
  ): Impl2[From, To.type, To] = {
    new Impl2[From, To.type, To] {
      def apply(v: From, v2: To.type): To = conv(v)
    }
  }

}
