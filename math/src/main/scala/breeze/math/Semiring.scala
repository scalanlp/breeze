package breeze.math

import breeze.storage.DefaultArrayValue

/**
 *
 * @author dlwh
 */
trait Semiring[@specialized(Int,Short,Long,Float,Double) V] extends Serializable {
  def zero : V

  def one : V

  def +(a : V, b : V) : V
  def *(a : V, b : V) : V

  def ==(a : V, b : V) : Boolean
  def !=(a : V, b : V) : Boolean
  def close(a: V, b: V, tolerance: Double=1E-4):Boolean = a == b

  /** Returns the class manifest of the scalar type. */
  def manifest : ClassManifest[V]

  /** Returns the DefaultArrayValue for this type.  Always this.zero. */
  def defaultArrayValue : DefaultArrayValue[V]
}

object Semiring {
  import Ring._
  implicit val semiringD: Semiring[Double] = ringD
  implicit val semiringFloat: Semiring[Float] = ringFloat
  implicit val semiringInt: Semiring[Int] = ringInt
  implicit val semiringLong: Semiring[Long] = ringLong
  implicit val semiringShort: Semiring[Short] = ringShort

  implicit object fieldB extends Semiring[Boolean] {
    def zero = false
    def one = true
    def nan = throw new ArithmeticException("Operation resulted in boolean-valued NaN")
    def ==(a : Boolean, b : Boolean) = a == b
    def !=(a : Boolean, b : Boolean) = a != b
    def +(a : Boolean, b : Boolean) = a || b
    def *(a : Boolean, b : Boolean) = a && b
    def norm(a : Boolean) = breeze.numerics.I(a)
    def toDouble(a : Boolean) = breeze.numerics.I(a)
    def isNaN(a : Boolean) = false
    def manifest = implicitly[ClassManifest[Boolean]]
    val defaultArrayValue = implicitly[DefaultArrayValue[Boolean]]
 }
}