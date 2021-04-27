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
case class Zero[@specialized T](zero: T) extends Serializable

object Zero {
//  def forClass(clazz: Class[_]): Zero[_] = {
//    if (clazz == Integer.TYPE) IntZero
//    else if (clazz == java.lang.Float.TYPE) FloatZero
//    else if (clazz == java.lang.Double.TYPE) DoubleZero
//    else if (clazz == java.lang.Short.TYPE) ShortZero
//    else if (clazz == java.lang.Byte.TYPE) ByteZero
//    else if (clazz == java.lang.Boolean.TYPE) BooleanZero
//    else if (clazz == java.lang.Character.TYPE) CharZero
//    else refDefault
//  }

  implicit val IntZero: Zero[Int] = Zero(0)
  implicit val ShortZero: Zero[Short] = Zero(0.toShort)
  implicit val LongZero: Zero[Long] = Zero(0L)
  implicit val ByteZero: Zero[Byte] = Zero(0.toByte)
  implicit val CharZero: Zero[Char] = Zero(0.toChar)
  implicit val FloatZero: Zero[Float] = Zero(0.0f)
  implicit val DoubleZero: Zero[Double] = Zero(0.0)
  implicit val BooleanZero: Zero[Boolean] = Zero(false)
  implicit val BigIntZero: Zero[BigInt] = Zero(BigInt(0))
  implicit val BigDecimalZero: Zero[BigDecimal] = Zero(BigDecimal(0L))
  // TODO: we could either add a multiplicative monoid type and put strings there
  // ... or go with the String semiring with concat as multiplication and longest common prefix as addition
  implicit val StringZero: Zero[String] = Zero("")

  implicit def zeroFromSemiring[T](implicit ring: Semiring[T]): Zero[T] = Zero(ring.zero)

  val refDefault: Zero[Null] = Zero(null)

}
