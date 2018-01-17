package breeze.collection.mutable

import scala.reflect.ClassTag

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
 * An ArrayLike is something that can behave kind of like an Array, but isn't.
 * They have a fixed size and reasonably fast access times. The main
 * difference is that they're sparse in one way or another.
 * This includes [[breeze.collection.mutable.OpenAddressHashArray]]s and
 * [[breeze.collection.mutable.SparseArray]]. They support several reasonable
 * operations
 *
 * @author dlwh
 */
// TODO: perhaps these should be called sparse Arrays
trait ArrayLike[V] {
  def apply(i: Int): V
  def update(i: Int, t: V): Unit

  /**
   * Only iterates "active" elements
   */
  def valuesIterator: Iterator[V]

  /**
   * Only iterates "active" keys
   */
  def keysIterator: Iterator[Int]

  /** Mainly for marking the underlying data array extent in SparseVector/SparseArray
   */
  def activeSize: Int

  def size: Int

  def length = size

  /**
   * Only iterates "active" elements. I'm not sure how I feel
   * about this behavior, since it's inconsistent with the rest of Breeze.
   * I will think on it.
   * @param f
   * @tparam U
   */
  // TODO: maybe make this iterate all elements?
  def foreach[U](f: (V) => U) = valuesIterator.foreach(f)

  /**
   * Only iterates "active" elements
   */
  def iterator = keysIterator.zip(valuesIterator)

  def toArray[U >: V: ClassTag] = Array.tabulate[U](length)(apply)

  def toList = List.tabulate(length)(apply)

  def toIndexedSeq = List.tabulate(length)(apply)

  def toMap = keysIterator.zip(valuesIterator).toMap
}
