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
import spire.implicits._

trait MeasuresCloseness[T] {
   def close(a: T, b: T, tolerance: Double): Boolean
}

trait FakeField[T] extends Field[T]
class FakeFieldFromRing[T](ring: spire.algebra.EuclideanRing[T]) extends FakeField[T] {
  // This is so wrong
  def zero = ring.zero
  def one = ring.one
  def plus(a: T, b: T) = ring.plus(a,b)
  def times(a: T, b: T) = ring.times(a,b)
  def negate(a: T) = ring.negate(a)
  def div(a: T, b: T) = ring.quot(a,b)
  def mod(a: T, b: T) = ring.mod(a,b)
  def gcd(a: T, b: T) = ring.gcd(a,b)
  def quot(a: T, b: T) = ring.quot(a,b)
}

trait TemporaryTranslation {
  implicit class TemporaryFieldTranslation[T](ring: spire.algebra.EuclideanRing[T]) extends Ring[T] {
    //Here to translate Spire fields into Breeze rings, first step in eliminating breeze.math
    def zero = ring.zero
    def one = ring.one
    def +(a: T, b: T) = ring.plus(a,b)
    def *(a: T, b: T) = ring.times(a,b)
    def -(a: T, b: T) = ring.minus(a,b)
    def ==(a: T, b: T): Boolean = (a == b)
    def !=(a: T, b: T): Boolean = (a != b)
  }
}

object BreezeFields {

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldInt extends FakeFieldFromRing(spire.std.int.IntAlgebra)

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldShort extends FakeFieldFromRing(spire.std.short.ShortAlgebra)

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldLong extends FakeFieldFromRing(spire.std.long.LongAlgebra)

  /** Not a field, but whatever. */
  @SerialVersionUID(1L)
  implicit object fieldBigInt extends FakeFieldFromRing(spire.std.bigInt.BigIntAlgebra)
}
