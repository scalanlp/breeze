package breeze.linalg
package support
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
 * Class that is kind of like a collection view of the keys in a tensor.
 * @author dlwh
 */
class TensorKeys[K, V, +This](private val tensor: This, active: Boolean = false, f: K=>Boolean = { (k: K) => true})(implicit ev: This <:< Tensor[K, V]) {
  def size = tensor.size
  def iterator = {if(active) tensor.activeKeysIterator else tensor.keysIterator}.filter(f)

  def foreach[U](fn: K=>U) = iterator foreach fn
  def filter(p: K=>Boolean): TensorKeys[K, V, This] = withFilter(p)
  def withFilter(p: K=>Boolean): TensorKeys[K, V, This] = new TensorKeys[K, V, This](tensor, active, {(a:K) => f(a) && p(a)})(ev)

  override def toString = iterator.mkString("TensorKeys(",",",")")

  override def equals(p1: Any) = p1 match {
    case x: TensorKeys[_, _, _] => x.eq(this) || iterator.sameElements(x.iterator)
    case _ => false
  }


}
