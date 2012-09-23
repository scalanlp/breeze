package breeze.storage

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

@SerialVersionUID(1l)
trait DefaultArrayValue[@specialized T] extends Serializable {
  def value : T
}

object DefaultArrayValue {
  def forClass(clazz: Class[_]):DefaultArrayValue[_] = {
    if(clazz == Integer.TYPE) IntDefaultArrayValue
    else if (clazz == java.lang.Float.TYPE) FloatDefaultArrayValue
    else if (clazz == java.lang.Double.TYPE) DoubleDefaultArrayValue
    else if (clazz == java.lang.Short.TYPE) ShortDefaultArrayValue
    else if (clazz == java.lang.Byte.TYPE) ByteDefaultArrayValue
    else if (clazz == java.lang.Boolean.TYPE) BooleanDefaultArrayValue
    else if (clazz == java.lang.Character.TYPE) CharDefaultArrayValue
    else refDefault
  }

  def apply[T](v: T):DefaultArrayValue[T] = new DefaultArrayValue[T] {
    def value = v
  }

  implicit object IntDefaultArrayValue extends DefaultArrayValue[Int] {
    override def value = 0
  }

  implicit object ShortDefaultArrayValue extends DefaultArrayValue[Short] {
    override def value = 0.toShort
  }

  implicit object LongDefaultArrayValue extends DefaultArrayValue[Long] {
    override def value = 0l
  }

  implicit object ByteDefaultArrayValue extends DefaultArrayValue[Byte] {
    override def value = 0.toByte
  }

  implicit object CharDefaultArrayValue extends DefaultArrayValue[Char] {
    override def value = 0.toChar
  }

  implicit object FloatDefaultArrayValue extends DefaultArrayValue[Float] {
    override def value = 0.0f
  }

  implicit object DoubleDefaultArrayValue extends DefaultArrayValue[Double] {
    override def value = 0.0
  }

  implicit object BooleanDefaultArrayValue extends DefaultArrayValue[Boolean] {
    override def value = false
  }

  val refDefault = new DefaultArrayValue[AnyRef] {
    override def value : AnyRef = null
  }

  implicit def ObjectDefaultArrayValue[T<:AnyRef] =
    refDefault.asInstanceOf[DefaultArrayValue[T]]
}

