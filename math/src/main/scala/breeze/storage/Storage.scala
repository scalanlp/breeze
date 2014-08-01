package breeze.storage

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
 * Interface for an unboxed iterable, of sorts, for
 * things backed by a flat array of elements.
 *
 * @author dlwh
 */
trait Storage[@specialized(Int,Double,Float,Long) E] {
  /**
   * Returns the actual flat array of elements used.
   * @return
   */
  def data: Array[E]

  /**
   * How many elements are logically stored here. This may be <= activeSize.
   * @return
   */
  protected def size: Int

  /**
   * How many elements are stored in terms of space.
   *
   * @return
   */
  def activeSize: Int

  //ToDo: clarify and document---redundant with activeSize?
  /**
   * How many elements must be iterated over using valueAt/indexAt.
   * @return
   */
  def iterableSize: Int = activeSize

  /**
   * same as data(i). Gives the value at the underlying offset.
   * @param i index into the data array
   * @return
   */
  def valueAt(i: Int): E

  /**
   * Gives the logical index from the physical index.
   * @param i
   * @return
   */
  def indexAt(i: Int): Int

  /**
   * Some storages (namely HashStorage) won't have active
   * indices packed. This lets you know if the bin is
   * actively in use.
   * @param i index into index/data arrays
   * @return
   */
  def isActive(i: Int):Boolean

  /**
   * Only gives true if isActive would return true for all i. (May be false anyway)
   * @return
   */
  def allVisitableIndicesActive:Boolean
}
