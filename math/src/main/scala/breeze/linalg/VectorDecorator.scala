package breeze.linalg
/*
 Copyright 2012 Keith Stevens

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

import scala.{specialized=>spec}


class VectorDecorator[@spec(Double, Int, Float) E](val vector: Vector[E]) extends Vector[E] {

  def activeIterator = vector.iterator

  def activeSize = vector.length

  def activeKeysIterator = vector.keysIterator

  def activeValuesIterator = vector.valuesIterator

  def apply(i: Int) = vector.apply(i)

  def copy = vector.copy

  def length = vector.length

  def repr = vector.repr

  def update(i: Int, v: E) { vector.update(i, v) }
}
