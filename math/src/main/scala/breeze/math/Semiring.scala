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
  implicit val semiringCmplx: Semiring[Complex] = ringComplex

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