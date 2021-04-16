package breeze.storage

import breeze.math.Semiring

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
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
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
trait Zero[@specialized T] extends Serializable {
  def zero: T
}

object Zero extends ZeroLowPriority {
  def forClass(clazz: Class[_]): Zero[_] = {
    if (clazz == Integer.TYPE) IntZero
    else if (clazz == java.lang.Float.TYPE) FloatZero
    else if (clazz == java.lang.Double.TYPE) DoubleZero
    else if (clazz == java.lang.Short.TYPE) ShortZero
    else if (clazz == java.lang.Byte.TYPE) ByteZero
    else if (clazz == java.lang.Boolean.TYPE) BooleanZero
    else if (clazz == java.lang.Character.TYPE) CharZero
    else refDefault
  }

  def apply[T](v: T): Zero[T] = new Zero[T] {
    def zero = v
  }

  @SerialVersionUID(1L)
  implicit object IntZero extends Zero[Int] {
    override def zero = 0
  }

  @SerialVersionUID(1L)
  implicit object ShortZero extends Zero[Short] {
    override def zero = 0.toShort
  }

  @SerialVersionUID(1L)
  implicit object LongZero extends Zero[Long] {
    override def zero = 0L
  }

  @SerialVersionUID(1L)
  implicit object ByteZero extends Zero[Byte] {
    override def zero = 0.toByte
  }

  @SerialVersionUID(1L)
  implicit object CharZero extends Zero[Char] {
    override def zero = 0.toChar
  }

  @SerialVersionUID(1L)
  implicit object FloatZero extends Zero[Float] {
    override def zero = 0.0f
  }

  @SerialVersionUID(1L)
  implicit object DoubleZero extends Zero[Double] {
    override def zero = 0.0
  }

  @SerialVersionUID(1L)
  implicit object BooleanZero extends Zero[Boolean] {
    override def zero = false
  }

  @SerialVersionUID(1L)
  implicit object BigIntZero extends Zero[BigInt] {
    override def zero = BigInt(0)
  }

  @SerialVersionUID(1L)
  implicit object BigDecimalZero extends Zero[BigDecimal] {
    override def zero = BigDecimal(0L)
  }

}

trait ZeroVeryLowPriority {  self: Zero.type =>
  implicit def ObjectZero[T <: AnyRef]: Zero[T] = {
    refDefault.asInstanceOf[Zero[T]]
  }

  protected val refDefault: Zero[AnyRef] = new Zero[AnyRef] {
    override def zero: AnyRef = null
  }
}

trait ZeroLowPriority extends ZeroVeryLowPriority { self: Zero.type =>

  implicit def zeroFromSemiring[T: Semiring]: Zero[T] = Zero(implicitly[Semiring[T]].zero)

}
