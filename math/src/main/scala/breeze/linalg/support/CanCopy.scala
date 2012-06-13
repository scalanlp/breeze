package breeze.linalg.support

import breeze.math.Field
import actors.threadpool.Arrays
import breeze.util.ArrayUtil
import breeze.generic.CanMapValues

/**
 *
 * @author dlwh
 */
/**
 * Marker for being able to copy a collection
 *
 * @author dlwh
 */
trait CanCopy[T] {
  // Should not inherit from T=>T because those get  used by the compiler.
  def apply(t: T):T
}

object CanCopy {

  class OpArray[@specialized V:ClassManifest:Field]
  extends CanCopy[Array[V]] {
    override def apply(from : Array[V]) = {
      ArrayUtil.copyOf(from, from.length)
    }
  }

  class OpMapValues[From,A](implicit op : CanCopy[A], map : CanMapValues[From,A,A,From]) extends CanCopy[From] {
    def apply(v : From) = map.map(v, op.apply(_))
  }

  implicit def opMapValues[From,A](implicit map : CanMapValues[From,A,A,From], op : CanCopy[A])
  : CanCopy[From] = new OpMapValues[From,A]()(op, map)

  implicit def OpArrayAny[V:ClassManifest:Field] : OpArray[V] =
    new OpArray[V]

  implicit object OpArrayI extends OpArray[Int]
  implicit object OpArrayS extends OpArray[Short]
  implicit object OpArrayL extends OpArray[Long]
  implicit object OpArrayF extends OpArray[Float]
  implicit object OpArrayD extends OpArray[Double]

  implicit def canCopyField[V:Field]:CanCopy[V] = new CanCopy[V] {
    def apply(v1: V) = v1
  }
}
