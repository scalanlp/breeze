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
  * Trait marking a zero value in a particular specialized type (0, 0L, od, etc.).
  * Used in particular to retrieve implicit zero values particular types,
  * which are implicitly defined in the companion object to this trait.
  *
  * @author dlwh
  */
@SerialVersionUID(1l)
trait Zero[@specialized V] extends Serializable {
  def zero : V
}

object Zero extends ZeroLowPriority {

  def forClass(clazz: Class[_]):Zero[_] = {
    if(clazz == Integer.TYPE) IntZero
    else if (clazz == java.lang.Float.TYPE) FloatZero
    else if (clazz == java.lang.Double.TYPE) DoubleZero
    else if (clazz == java.lang.Short.TYPE) ShortZero
    else if (clazz == java.lang.Byte.TYPE) ByteZero
    else if (clazz == java.lang.Boolean.TYPE) BooleanZero
    else if (clazz == java.lang.Character.TYPE) CharZero
    else refDefault
  }

  def apply[V](v: V): Zero[V] = new Zero[V] {
    def zero = v
  }

  // <editor-fold defaultstate="collapsed" desc=" implicit Zero[V] definitions ">

  implicit object IntZero extends Zero[Int] {
    override def zero = 0
  }

  implicit object ShortZero extends Zero[Short] {
    override def zero = 0.toShort
  }

  implicit object LongZero extends Zero[Long] {
    override def zero = 0l
  }

  implicit object ByteZero extends Zero[Byte] {
    override def zero = 0.toByte
  }

  implicit object CharZero extends Zero[Char] {
    override def zero = 0.toChar
  }

  implicit object FloatZero extends Zero[Float] {
    override def zero = 0.0f
  }

  implicit object DoubleZero extends Zero[Double] {
    override def zero = 0.0
  }

  implicit object BooleanZero extends Zero[Boolean] {
    override def zero = false
  }

  implicit object BigIntZero extends Zero[BigInt] {
    override def zero = BigInt(0)
  }

  implicit object BigDecimalZero extends Zero[BigDecimal] {
    override def zero = BigDecimal(0L)
  }

  // </editor-fold>

}

trait ZeroVeryLowPriority { this: Zero.type =>

  implicit def ObjectZero[V <: AnyRef] = {
    refDefault.asInstanceOf[Zero[V]]
  }

  protected val refDefault = new Zero[AnyRef] {
    override def zero : AnyRef = null
  }

}

trait ZeroLowPriority extends ZeroVeryLowPriority { this: Zero.type =>

  implicit def zeroFromSemiring[V: Semiring] = Zero(implicitly[Semiring[V]].zero)

}