package breeze.math

import scala.annotation.implicitNotFound
import breeze.storage.DefaultArrayValue


/**
 * Marker trait for scalar values.  Scalars must be immutable.
 * TODO: maybe use spire for the basis of this?
 *
*  @author dlwh
 */
trait Field[@specialized(Int,Short,Long,Float,Double) V] extends Ring[V] {
  def /(a : V, b : V) : V
  def inverse(a: V) = /(one, a)

}

object Field {
  implicit object fieldInt extends Field[Int] {
    def zero = 0
    def one = 1
    def nan = throw new ArithmeticException("Operation resulted in integer-valued NaN")
    def ==(a : Int, b : Int) = a == b
    def !=(a : Int, b : Int) = a != b
    def +(a : Int, b : Int) = a + b
    def -(a : Int, b : Int) = a - b
    def *(a : Int, b : Int) = a * b
    def /(a : Int, b : Int) = a / b
    def norm(a : Int) = if (a < 0) -a else a
    def toDouble(a : Int) = a
    def isNaN(a : Int) = false
    val manifest = implicitly[ClassManifest[Int]]
    val defaultArrayValue = implicitly[DefaultArrayValue[Int]]


  }

  implicit object fieldShort extends Field[Short] {
    def zero = 0.asInstanceOf[Short]
    def one = 1.asInstanceOf[Short]
    def nan = throw new ArithmeticException("Operation resulted in short-valued NaN")
    def ==(a : Short, b : Short) = a == b
    def !=(a : Short, b : Short) = a != b
    def +(a : Short, b : Short) = (a + b).asInstanceOf[Short]
    def -(a : Short, b : Short) = (a - b).asInstanceOf[Short]
    def *(a : Short, b : Short) = (a * b).asInstanceOf[Short]
    def /(a : Short, b : Short) = (a / b).asInstanceOf[Short]
    def norm(a : Short) = if (a < 0) -a else a
    def toDouble(a : Short) = a
    def isNaN(a : Short) = false
    val manifest = implicitly[ClassManifest[Short]]
    val defaultArrayValue = implicitly[DefaultArrayValue[Short]]
  }

  implicit object fieldLong extends Field[Long] {
    def zero = 0l
    def one = 1l
    def nan = throw new ArithmeticException("Operation resulted in long-valued NaN")
    def ==(a : Long, b : Long) = a == b
    def !=(a : Long, b : Long) = a != b
    def +(a : Long, b : Long) = a + b
    def -(a : Long, b : Long) = a - b
    def *(a : Long, b : Long) = a * b
    def /(a : Long, b : Long) = a / b
    def norm(a : Long) = if (a < 0) -a else a
    def toDouble(a : Long) = a
    def isNaN(a : Long) = false
    val manifest = implicitly[ClassManifest[Long]]
    val defaultArrayValue = implicitly[DefaultArrayValue[Long]]
  }

  implicit object fieldFloat extends Field[Float] {
    def zero = 0.0f
    def one = 1.0f
    def nan = Float.NaN
    def ==(a : Float, b : Float) = a == b
    def !=(a : Float, b : Float) = a != b
    def +(a : Float, b : Float) = a + b
    def -(a : Float, b : Float) = a - b
    def *(a : Float, b : Float) = a * b
    def /(a : Float, b : Float) = a / b
    def norm(a : Float) = if (a < 0) -a else a
    def toDouble(a : Float) = a
    def isNaN(a : Float) = java.lang.Float.isNaN(a)
    val manifest = implicitly[ClassManifest[Float]]
    val defaultArrayValue = implicitly[DefaultArrayValue[Float]]

    override def close(a: Float, b: Float, tolerance: Double) = (a-b).abs <= math.max(a.abs, b.abs) * tolerance
  }

  implicit object fieldD extends Field[Double] {
    def zero = 0.0
    def one = 1.0
    def nan = Double.NaN
    def ==(a : Double, b : Double) = a == b
    def !=(a : Double, b : Double) = a != b
    def +(a : Double, b : Double) = a + b
    def -(a : Double, b : Double) = a - b
    def *(a : Double, b : Double) = a * b
    def /(a : Double, b : Double) = a / b
    def norm(a : Double) = if (a < 0) -a else a
    def toDouble(a : Double) = a
    def isNaN(a : Double) = java.lang.Double.isNaN(a)
    val manifest = implicitly[ClassManifest[Double]]
    val defaultArrayValue = implicitly[DefaultArrayValue[Double]]

    override def close(a: Double, b: Double, tolerance: Double) = (a-b).abs <= math.max(a.abs, b.abs) * tolerance
  }
}

