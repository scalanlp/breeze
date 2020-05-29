package breeze.math

import breeze.linalg.norm
import breeze.numerics
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

/**
 * Marker trait for scalar values.  Scalars must be immutable.
 * TODO: maybe use spire for the basis of this?
 *
 *  @author dlwh
 */
trait Field[@specialized(Int, Short, Long, Float, Double) V] extends Ring[V] {
  def /(a: V, b: V): V
  def inverse(a: V) = /(one, a)
  def pow(a: V, b: V): V
}

object Field {

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldInt extends Field[Int] with Serializable {
    def zero = 0
    def one = 1
    def ==(a: Int, b: Int) = a == b
    def !=(a: Int, b: Int) = a != b
    def +(a: Int, b: Int) = a + b
    def -(a: Int, b: Int) = a - b
    def *(a: Int, b: Int) = a * b
    def /(a: Int, b: Int) = a / b
    def %(a: Int, b: Int) = a % b
    def pow(a: Int, b: Int) = math.pow(a, b).toInt

    implicit val normImpl: norm.Impl[Int, Double] = new norm.Impl[Int, Double] {
      def apply(v: Int) = math.abs(v).toDouble
    }
  }

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldShort extends Field[Short] with Serializable {
    def zero = 0.asInstanceOf[Short]
    def one = 1.asInstanceOf[Short]
    def ==(a: Short, b: Short) = a == b
    def !=(a: Short, b: Short) = a != b
    def +(a: Short, b: Short) = (a + b).asInstanceOf[Short]
    def -(a: Short, b: Short) = (a - b).asInstanceOf[Short]
    def *(a: Short, b: Short) = (a * b).asInstanceOf[Short]
    def /(a: Short, b: Short) = (a / b).asInstanceOf[Short]
    def %(a: Short, b: Short) = (a % b).asInstanceOf[Short]
    def pow(a: Short, b: Short) = math.pow(a, b).toShort

    implicit val normImpl: norm.Impl[Short, Double] = new norm.Impl[Short, Double] {
      def apply(v: Short) = math.abs(v).toDouble
    }
  }

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldLong extends Field[Long] with Serializable {
    def zero = 0L
    def one = 1L
    def ==(a: Long, b: Long) = a == b
    def !=(a: Long, b: Long) = a != b
    def +(a: Long, b: Long) = a + b
    def -(a: Long, b: Long) = a - b
    def *(a: Long, b: Long) = a * b
    def /(a: Long, b: Long) = a / b
    def %(a: Long, b: Long) = a % b.toLong
    def pow(a: Long, b: Long) = math.pow(a, b).toLong

    implicit val normImpl: norm.Impl[Long, Double] = new norm.Impl[Long, Double] {
      def apply(v: Long) = math.abs(v).toDouble
    }
  }

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldBigInt extends Field[BigInt] with Serializable {
    def zero = 0L
    def one = 1L
    def ==(a: BigInt, b: BigInt) = a == b
    def !=(a: BigInt, b: BigInt) = a != b
    def +(a: BigInt, b: BigInt) = a + b
    def -(a: BigInt, b: BigInt) = a - b
    def *(a: BigInt, b: BigInt) = a * b
    def /(a: BigInt, b: BigInt) = a / b
    def %(a: BigInt, b: BigInt) = a % b
    def pow(a: BigInt, b: BigInt): BigInt = a.pow(b.toInt)

    implicit val normImpl: norm.Impl[BigInt, Double] = new norm.Impl[BigInt, Double] {
      def apply(v: BigInt) = v.abs.toDouble
    }
  }

  @SerialVersionUID(1L)
  implicit object fieldBigDecimal extends Field[BigDecimal] with Serializable {
    def zero = 0L
    def one = 1L
    def ==(a: BigDecimal, b: BigDecimal) = a == b
    def !=(a: BigDecimal, b: BigDecimal) = a != b
    def +(a: BigDecimal, b: BigDecimal) = a + b
    def -(a: BigDecimal, b: BigDecimal) = a - b
    def *(a: BigDecimal, b: BigDecimal) = a * b
    def /(a: BigDecimal, b: BigDecimal) = a / b
    def %(a: BigDecimal, b: BigDecimal) = a % b
    def pow(a: BigDecimal, b: BigDecimal): BigDecimal = a.pow(b.toInt)

    override def close(a: BigDecimal, b: BigDecimal, tolerance: Double): Boolean = {
      (a - b).abs <= tolerance * (a.abs.max(b.abs))
    }

    implicit val normImpl: norm.Impl[BigDecimal, Double] = new norm.Impl[BigDecimal, Double] {
      def apply(v: BigDecimal) = v.abs.toDouble
    }
  }

  @SerialVersionUID(1L)
  implicit object fieldFloat extends Field[Float] with Serializable {
    def zero = 0.0f
    def one = 1.0f
    def ==(a: Float, b: Float) = a == b
    def !=(a: Float, b: Float) = a != b
    def +(a: Float, b: Float) = a + b
    def -(a: Float, b: Float) = a - b
    def *(a: Float, b: Float) = a * b
    def /(a: Float, b: Float) = a / b
    def %(a: Float, b: Float) = a % b
    def pow(a: Float, b: Float) = numerics.pow(a, b)

    // http://floating-point-gui.de/errors/comparison/
    override def close(a: Float, b: Float, tolerance: Double) = {
      val diff = math.abs(a - b)
      a == b || (diff <= math.max(
        a.abs,
        b.abs
      ) * tolerance) || ((a == 0 || b == 0 || diff < java.lang.Float.MIN_NORMAL) && diff < tolerance * 10 * java.lang.Float.MIN_NORMAL)
    }

    implicit val normImpl: norm.Impl[Float, Double] = new norm.Impl[Float, Double] {
      def apply(v: Float) = math.abs(v).toDouble
    }
  }

  @SerialVersionUID(-5955467582882664220L)
  implicit object fieldDouble extends Field[Double] with Serializable {
    def zero = 0.0
    def one = 1.0
    def ==(a: Double, b: Double) = a == b
    def !=(a: Double, b: Double) = a != b
    def +(a: Double, b: Double) = a + b
    def -(a: Double, b: Double) = a - b
    def *(a: Double, b: Double) = a * b
    def /(a: Double, b: Double) = a / b
    def %(a: Double, b: Double): Double = a % b
    def pow(a: Double, b: Double): Double = math.pow(a, b)

    override def close(a: Double, b: Double, tolerance: Double) = {
      val diff = math.abs(a - b)
      a == b || (diff <= math.max(
        a.abs,
        b.abs
      ) * tolerance) || ((a == 0 || b == 0 || diff < java.lang.Double.MIN_NORMAL) && diff < tolerance * 10 * java.lang.Double.MIN_NORMAL)
    }

    implicit val normImpl: norm.Impl[Double, Double] = new norm.Impl[Double, Double] {
      def apply(v: Double) = math.abs(v)
    }
  }
}
