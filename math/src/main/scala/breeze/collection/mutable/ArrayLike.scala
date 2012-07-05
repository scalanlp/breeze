package breeze.collection.mutable

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
 *
 * @author dlwh
 */
trait ArrayLike[T] {
  def apply(i: Int): T
  def update(i: Int, t: T): Unit

  /**
   * Only iterates "active" elements
   */
  def valuesIterator: Iterator[T]


  /**
   * Only iterates "active" keys
   */
  def keysIterator: Iterator[Int]

  def activeSize: Int

  def size: Int

  def length = size

  def foreach[U](f: (T) => U) = valuesIterator foreach f


  /**
   * Only iterates "active" elements
   */
  def iterator = keysIterator zip valuesIterator

  def toArray[U>:T:ClassManifest] = Array.tabulate[U](length)(apply)

  def toList = List.tabulate(length)(apply)

  def toIndexedSeq = List.tabulate(length)(apply)

  def toMap = (keysIterator zip valuesIterator).toMap
}