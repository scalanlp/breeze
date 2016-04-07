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
 * Class that is kind of like a collection view of the values in a tensor.
  *
 * @author dlwh
 */
class TensorValues[K, V, +This](private val tensor: This, active: Boolean = false, f: (V) => Boolean = { (x: Any) => true })(implicit ev: This <:< Tensor[K, V]) {
  def size = tensor.size

  def iterator = {if(active) tensor.activeValuesIterator else tensor.valuesIterator}.filter(f)

  def foreach[U](fn: V => U) = iterator foreach fn

//  def filter(p: V => Boolean) = withFilter(p)
//
//  def withFilter(p: (V) => Boolean): TensorValues[K, V, This] = {
//    new TensorValues[K, V, This](tensor, active, { a => f(a) && p(a) })(ev)
//  }

  override def toString = iterator.mkString("TensorValues(", ",", ")")

  override def equals(p1: Any) = p1 match {
    case x: TensorValues[_, _, _] => x.eq(this) || iterator.sameElements(x.iterator)
    case _ => false
  }

  def map[TT>:This,O,That](fn : (V) => O)
                          (implicit bf : CanMapValues[TT, V, O, That]) : That = {
    tensor.mapValues(fn)(bf.asInstanceOf[CanMapValues[Tensor[K, V], V, O, That]])
  }

  def exists(f: V=>Boolean) = iterator exists f
  def forall(f: V=>Boolean) = iterator forall f


 }
