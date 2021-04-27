package breeze.math

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
trait Semiring[@specialized(Int, Short, Long, Float, Double) V] extends Serializable {
  def zero: V

  def one: V

  def +(a: V, b: V): V
  def *(a: V, b: V): V

  def ==(a: V, b: V): Boolean
  def !=(a: V, b: V): Boolean
  def close(a: V, b: V, tolerance: Double = 1E-4): Boolean = a == b

}

object Semiring {
  import breeze.math.Ring._
  implicit def semiringD: Semiring[Double] = ringD
  implicit def semiringFloat: Semiring[Float] = ringFloat
  implicit def semiringInt: Semiring[Int] = ringInt
  implicit def semiringLong: Semiring[Long] = ringLong
  implicit def semiringBigInt: Semiring[BigInt] = ringBigInt
  implicit def semiringShort: Semiring[Short] = ringShort
  implicit def semiringCmplx: Semiring[Complex] = ringComplex

  implicit def semiringFromRing[T](implicit ring: Ring[T]): Semiring[T] = ring

  @SerialVersionUID(1L)
  implicit object fieldB extends Semiring[Boolean] {
    def zero = false
    def one = true
    def ==(a: Boolean, b: Boolean) = a == b
    def !=(a: Boolean, b: Boolean) = a != b
    def +(a: Boolean, b: Boolean) = a || b
    def *(a: Boolean, b: Boolean) = a && b
  }
}
