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

trait Ring[@specialized(Int,Short,Long,Float,Double) V] extends Semiring[V]  {

  def -(a : V, b : V) : V
  def negate(s: V): V = this.-(zero, s)

  /** Returns true if this is a primitive type. */
  def isPrimitive : Boolean = manifest.runtimeClass.isPrimitive

  /** Returns true if this is not a number. */
  def isNaN(a : V) : Boolean

}

object Ring {
  import Field._
  implicit val ringD: Ring[Double] = fieldD
  implicit val ringFloat: Ring[Float] = fieldFloat
  implicit val ringInt: Ring[Int] = fieldInt
  implicit val ringLong: Ring[Long] = fieldLong
  implicit val ringBigInt: Ring[BigInt] = fieldBigInt
  implicit val ringShort: Ring[Short] = fieldShort
  implicit val ringComplex: Ring[Complex] = Complex.scalar
}
