package breeze.linalg.support
/*
 Copyright 2012 Daniel Ramage

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
import breeze.math.{Semiring, Field}
import scala.reflect.ClassTag


/**
 * Marker for being able to create a collection of the same shape as
 * the given input but with zero values everywhere.
 *
 * @author dramage
 */
trait CanCreateZerosLike[From, +To] {
  // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
  def apply(from: From):To
}

object CanCreateZerosLike {
  implicit def canCreateScalarZeroes[@specialized(Int,Short,Long,Float,Double) V:Semiring] = new CanCreateZerosLike[V, V] {
    def apply(d: V): V = implicitly[Semiring[V]].zero
  }

  class OpArray[@specialized V:ClassTag:Semiring]
  extends CanCreateZerosLike[Array[V],Array[V]] {
    override def apply(from : Array[V]) = {
      Array.fill(from.length)(implicitly[Semiring[V]].zero)
    }
  }

  class OpMapValues[From,A,To](implicit op : Semiring[A], map : CanMapValues[From,A,A,To]) extends CanCreateZerosLike[From,To] {
    def apply(v : From) = map(v, _ => op.zero)
  }

  implicit def opMapValues[From,A,To](implicit map : CanMapValues[From,A,A,To], op : Field[A])
  : CanCreateZerosLike[From,To] = new OpMapValues[From,A,To]()(op, map)

  implicit def OpArrayAny[V:ClassTag:Semiring] : OpArray[V] =
    new OpArray[V]

  implicit object OpArrayI extends OpArray[Int]
  implicit object OpArrayS extends OpArray[Short]
  implicit object OpArrayL extends OpArray[Long]
  implicit object OpArrayF extends OpArray[Float]
  implicit object OpArrayD extends OpArray[Double]
}

