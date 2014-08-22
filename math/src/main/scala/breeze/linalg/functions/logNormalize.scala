package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.operators.OpSub
import breeze.numerics

object logNormalize extends UFunc {
  implicit def logNormalizeImpl[V](implicit softmaxImpl: softmax.Impl[V, Double],
                                   op : OpSub.Impl2[V, Double, V]):Impl[V, V] = new Impl[V, V] {

    def apply(value: V): V = {
      val max = softmax(value)
      if(max == Double.NegativeInfinity) value
      else op(value, max)
    }
  }
}

object logAndNormalize extends UFunc {
  implicit def logNormalizeImpl[V](implicit logImpl: breeze.numerics.log.Impl[V, V],
                                   logNormalizeImpl: logNormalize.Impl[V, V]):Impl[V, V] = new Impl[V, V] {

    def apply(value: V): V = {
      logNormalize(numerics.log(value))
    }
  }

}
