package breeze.math

import breeze.linalg

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
trait Ring[@specialized(Int, Short, Long, Float, Double) V] extends Semiring[V] {

  def -(a: V, b: V): V
  def negate(s: V): V = this.-(zero, s)
  def %(a: V, b: V): V

  implicit val normImpl: linalg.norm.Impl[V, Double]
  def sNorm(a: V): Double = linalg.norm(a)
}

object Ring {
  import Field._
  implicit def ringD: Ring[Double] = fieldDouble
  implicit def ringFloat: Ring[Float] = fieldFloat
  implicit def ringInt: Ring[Int] = fieldInt
  implicit def ringLong: Ring[Long] = fieldLong
  implicit def ringBigInt: Ring[BigInt] = fieldBigInt
  implicit def ringShort: Ring[Short] = fieldShort
  implicit def ringComplex: Ring[Complex] = Complex.scalar

  implicit def ringFromField[T](implicit field: Field[T]): Ring[T] = field
}
