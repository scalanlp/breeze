package breeze.linalg.support
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
import breeze.math.Field
import actors.threadpool.Arrays
import breeze.util.ArrayUtil
import breeze.generic.CanMapValues

/**
 * Marker for being able to copy a collection
 *
 * @author dlwh
 */
trait CanCopy[T] {
  // Should not inherit from T=>T because those get  used by the compiler.
  def apply(t: T):T
}

object CanCopy {

  class OpArray[@specialized V:ClassManifest:Field]
  extends CanCopy[Array[V]] {
    override def apply(from : Array[V]) = {
      ArrayUtil.copyOf(from, from.length)
    }
  }

  class OpMapValues[From,A](implicit op : CanCopy[A], map : CanMapValues[From,A,A,From]) extends CanCopy[From] {
    def apply(v : From) = map.map(v, op.apply(_))
  }

  implicit def opMapValues[From,A](implicit map : CanMapValues[From,A,A,From], op : CanCopy[A])
  : CanCopy[From] = new OpMapValues[From,A]()(op, map)

  implicit def OpArrayAny[V:ClassManifest:Field] : OpArray[V] =
    new OpArray[V]

  implicit object OpArrayI extends OpArray[Int]
  implicit object OpArrayS extends OpArray[Short]
  implicit object OpArrayL extends OpArray[Long]
  implicit object OpArrayF extends OpArray[Float]
  implicit object OpArrayD extends OpArray[Double]

  implicit def canCopyField[V:Field]:CanCopy[V] = new CanCopy[V] {
    def apply(v1: V) = v1
  }
}
