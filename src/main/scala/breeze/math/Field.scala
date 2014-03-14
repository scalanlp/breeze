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


/**
 * Marker trait for scalar values.  Scalars must be immutable.
 * TODO: maybe use spire for the basis of this?
 *
*  @author dlwh
 */
import spire.algebra.Field

trait MeasuresCloseness[T] {
   def close(a: T, b: T, tolerance: Double): Boolean
}

trait FakeField[T] extends Field[T]

trait TemporaryTranslation {
  implicit class TemporaryFieldTranslation[T](ring: spire.algebra.Ring[T]) extends Ring[T] {
    //Here to translate Spire fields into Breeze rings, first step in eliminating breeze.math
    def zero = ring.zero
    def one = ring.one
    def +(a: T, b: T) = ring.plus(a,b)
    def *(a: T, b: T) = ring.times(a,b)
  }
}

object BreezeFields {

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldInt extends FakeField[Int] {
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
  implicit object fieldShort extends FakeField[Short] {
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
  implicit object fieldLong extends FakeField[Long] {
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
  implicit object fieldBigInt extends FakeField[BigInt] {
    def zero = 0l
    def one = 1l
    def ==(a : BigInt, b : BigInt) = a == b
    def !=(a : BigInt, b : BigInt) = a != b
    def +(a : BigInt, b : BigInt) = a + b
    def -(a : BigInt, b : BigInt) = a - b
    def *(a : BigInt, b : BigInt) = a * b
    def /(a : BigInt, b : BigInt) = a / b
  }
}
