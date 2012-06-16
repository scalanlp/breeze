package breeze.math

/**
 *
 * @author dlwh
 */

trait Ring[@specialized(Int,Short,Long,Float,Double) V] extends Semiring[V]  {

  def -(a : V, b : V) : V
  def negate(s: V): V = this.-(zero, s)

  /** Returns true if this is a primitive type. */
  def isPrimitive : Boolean =
    ! (manifest <:< implicitly[Manifest[AnyRef]])

  /** Returns the norm of this value, the absolute value as a Double. */
  def norm(a : V) : Double

  /** Returns true if this is not a number. */
  def isNaN(a : V) : Boolean

}

object Ring {
  import Field._
  implicit val ringD: Ring[Double] = fieldD
  implicit val ringFloat: Ring[Float] = fieldFloat
  implicit val ringInt: Ring[Int] = fieldInt
  implicit val ringLong: Ring[Long] = fieldLong
  implicit val ringShort: Ring[Short] = fieldShort
}
