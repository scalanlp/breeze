package breeze.math
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
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag


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
  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldInt extends Field[Int] {
    def zero = 0
    def one = 1
    def ==(a : Int, b : Int) = a == b
    def !=(a : Int, b : Int) = a != b
    def +(a : Int, b : Int) = a + b
    def -(a : Int, b : Int) = a - b
    def *(a : Int, b : Int) = a * b
    def /(a : Int, b : Int) = a / b
  }

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldShort extends Field[Short] {
    def zero = 0.asInstanceOf[Short]
    def one = 1.asInstanceOf[Short]
    def ==(a : Short, b : Short) = a == b
    def !=(a : Short, b : Short) = a != b
    def +(a : Short, b : Short) = (a + b).asInstanceOf[Short]
    def -(a : Short, b : Short) = (a - b).asInstanceOf[Short]
    def *(a : Short, b : Short) = (a * b).asInstanceOf[Short]
    def /(a : Short, b : Short) = (a / b).asInstanceOf[Short]
  }

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldLong extends Field[Long] {
    def zero = 0l
    def one = 1l
    def ==(a : Long, b : Long) = a == b
    def !=(a : Long, b : Long) = a != b
    def +(a : Long, b : Long) = a + b
    def -(a : Long, b : Long) = a - b
    def *(a : Long, b : Long) = a * b
    def /(a : Long, b : Long) = a / b
  }

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldBigInt extends Field[BigInt] {
    def zero = 0l
    def one = 1l
    def ==(a : BigInt, b : BigInt) = a == b
    def !=(a : BigInt, b : BigInt) = a != b
    def +(a : BigInt, b : BigInt) = a + b
    def -(a : BigInt, b : BigInt) = a - b
    def *(a : BigInt, b : BigInt) = a * b
    def /(a : BigInt, b : BigInt) = a / b
  }

  @SerialVersionUID(1L)
  implicit object fieldFloat extends Field[Float] {
    def zero = 0.0f
    def one = 1.0f
    def ==(a : Float, b : Float) = a == b
    def !=(a : Float, b : Float) = a != b
    def +(a : Float, b : Float) = a + b
    def -(a : Float, b : Float) = a - b
    def *(a : Float, b : Float) = a * b
    def /(a : Float, b : Float) = a / b

    override def close(a: Float, b: Float, tolerance: Double) = (a-b).abs <= math.max(a.abs, b.abs) * tolerance
  }

  @SerialVersionUID(-5955467582882664220L)
  implicit object fieldD extends Field[Double] {
    def zero = 0.0
    def one = 1.0
    def ==(a : Double, b : Double) = a == b
    def !=(a : Double, b : Double) = a != b
    def +(a : Double, b : Double) = a + b
    def -(a : Double, b : Double) = a - b
    def *(a : Double, b : Double) = a * b
    def /(a : Double, b : Double) = a / b

    override def close(a: Double, b: Double, tolerance: Double) = (a-b).abs <= math.max(a.abs, b.abs) * tolerance
  }
}

