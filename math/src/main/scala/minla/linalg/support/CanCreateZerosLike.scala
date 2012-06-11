package minla.linalg.support

import minla.hierarchy.Field

/**
 *
 * @author dlwh
 */

/**
 * Marker for being able to create a collection of the same shape as
 * the given input but with zero values everywhere.
 *
 * @author dramage
 */
trait CanCreateZerosLike[-From, +To] {
  // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
  def apply(from: From):To
}

object CanCreateZerosLike {

  class OpArray[@specialized V:ClassManifest:Field]
  extends CanCreateZerosLike[Array[V],Array[V]] {
    override def apply(from : Array[V]) = {
      Array.fill(from.length)(implicitly[Field[V]].zero)
    }
  }

  class OpMapValues[From,A,To](implicit op : Field[A], map : CanMapValues[From,A,A,To]) extends CanCreateZerosLike[From,To] {
    def apply(v : From) = map.map(v, _ => op.zero)
  }

  implicit def opMapValues[From,A,To](implicit map : CanMapValues[From,A,A,To], op : Field[A])
  : CanCreateZerosLike[From,To] = new OpMapValues[From,A,To]()(op, map)

  implicit def OpArrayAny[V:ClassManifest:Field] : OpArray[V] =
    new OpArray[V]

  implicit object OpArrayI extends OpArray[Int]
  implicit object OpArrayS extends OpArray[Short]
  implicit object OpArrayL extends OpArray[Long]
  implicit object OpArrayF extends OpArray[Float]
  implicit object OpArrayD extends OpArray[Double]
}

