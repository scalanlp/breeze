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

import java.util

import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3

import breeze.storage.{ConfigurableDefault, Storage, Zero}

/**
 * This is a Sparse Array implementation backed by a linear-probing
 * open address hash table.
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
final class OpenAddressHashArray[@specialized(Int, Float, Long, Double) V] private[mutable] (
    private[mutable] var _index: Array[Int],
    private[mutable] var _data: Array[V],
    private[mutable] var load: Int,
    val size: Int,
    val default: ConfigurableDefault[V] = ConfigurableDefault.default[V])(
    implicit protected val manElem: ClassTag[V],
    val zero: Zero[V])
    extends Storage[V]
    with ArrayLike[V]
    with Serializable {

  require(size > 0, "Size must be positive, but got " + size)

  def this(size: Int, default: ConfigurableDefault[V], initialSize: Int)(implicit manElem: ClassTag[V], zero: Zero[V]) = {
    this(
      OpenAddressHashArray.emptyIndexArray(OpenAddressHashArray.calculateSize(initialSize)),
      default.makeArray(OpenAddressHashArray.calculateSize(initialSize)),
      0,
      size,
      default
    )
  }

  def this(size: Int, default: ConfigurableDefault[V])(implicit manElem: ClassTag[V], zero: Zero[V]) = {
    this(size, default, 16)
  }

  def this(size: Int)(implicit manElem: ClassTag[V], zero: Zero[V]) = {
    this(size, ConfigurableDefault.default[V])
  }

  def data = _data
  def index = _index

  def defaultValue = default.value(zero)

  /**
   * Only iterates "active" elements
   */
  def valuesIterator = activeValuesIterator

  def valueAt(i: Int) = data(i)
  def indexAt(i: Int) = index(i)

  def keysIterator = index.iterator.filter(_ >= 0)

  def activeSize = load

  def contains(i: Int) = index(locate(i)) >= 0

  def isActive(i: Int) = index(i) >= 0

  def allVisitableIndicesActive = false

  final def apply(i: Int) = {
    if (i < 0 || i >= size) throw new IndexOutOfBoundsException()
    if (index.length == 0) default.value
    else data(locate(i))
  }

  final def update(i: Int, v: V): Unit = {
    if (i < 0 || i >= size) throw new IndexOutOfBoundsException(s"$i is out of bounds for size $size")
    val pos = locate(i)
    _data(pos) = v
    if (_index(pos) != i && v != defaultValue) {
      load += 1
      if (load * 4 > _index.length * 3) {
        rehash()
        update(i, v)
      } else {
        _index(pos) = i
      }
    }

  }

  def activeKeysIterator = keysIterator
  def activeValuesIterator = activeIterator.map(_._2)
  def activeIterator = index.iterator.zip(data.iterator).filter(_._1 >= 0)

  private def locate(i: Int) = {
    if (i >= size) throw new IndexOutOfBoundsException(s"$i greater than size of $size")
    if (i < 0) throw new IndexOutOfBoundsException(s"$i less than 0")
    val index = this.index
    val len = index.length
    var hash = hashCodeFor(i) & (len - 1)
    while (index(hash) != i && index(hash) >= 0) {
      hash += 1
      if (hash >= len) {
        hash = 0
      }
    }
    hash
  }

  private def hashCodeFor(i: Int): Int = {
    // based on what's in HashTable.scala and scala.util.hashing (inlined because of 2.9.2 support)
    // doing this so that i and i +1 aren't near each other.
    var code = i.##
    code *= 0x9e3775cd
    code = java.lang.Integer.reverseBytes(code)
    code *= 0x9e3775cd
    val rotated = (code >>> 11) | (code << 21)
    rotated
  }

  final protected def rehash(): Unit = {
    val oldIndex = index
    val oldValues = data
    val newSize = OpenAddressHashArray.calculateSize(oldIndex.size + 1)
    _index = new Array[Int](newSize)
    util.Arrays.fill(_index, -1)
    _data = new Array[V](newSize)
    default.fillArray(_data, default.value)
    load = 0
    var i = 0
    while (i < oldIndex.length) {
      if (oldIndex(i) >= 0) {
        update(oldIndex(i), oldValues(i))
      }
      i += 1
    }
  }

  /**
   * How many elements must be iterated over using valueAt/indexAt.
   * @return
   */
  override def iterableSize = index.length

  override def toString: String = activeIterator.mkString("OpenAddressHashArray(", ", ", ")")

  def copy: OpenAddressHashArray[V] = {
    new OpenAddressHashArray[V](
      util.Arrays.copyOf(_index, _index.length),
      breeze.util.ArrayUtil.copyOf(_data, _data.length),
      load,
      size,
      default)
  }

  def copyTo(other: OpenAddressHashArray[V]): Unit = {
    require(other.length == other.length, "vectors must have the same length")
    require(defaultValue == other.defaultValue, "vectors must have the same default")
    other._index = _index.clone()
    other._data = _data.clone()
    other.load = load
  }

  def clear(): Unit = {
    _data = default.makeArray(16)
    _index = OpenAddressHashArray.emptyIndexArray(16)
    load = 0
  }

  // This hash code must be symmetric in the contents but ought not
  // collide trivially. based on hashmap.hashcode
  override def hashCode() = MurmurHash3.unorderedHash(iterator.filter(_._2 != default.value), 43)

  override def equals(that: Any): Boolean = that match {
    case that: OpenAddressHashArray[V @unchecked] =>
      (this eq that) ||
        (this.size == that.size) && {
          try {
            this.iterator.forall {
              case (k, v) =>
                that(k) match {
                  case `v` =>
                    true
                  case _ => false
                }
            }
          } catch {
            case ex: ClassCastException =>
              false
          }
        }
    case _ =>
      false
  }

}

object OpenAddressHashArray {
  def apply[@specialized(Int, Float, Long, Double) T: ClassTag: Zero](values: T*) = {
    val rv = new OpenAddressHashArray[T](values.length)
    val zero = implicitly[Zero[T]].zero
    for ((v, i) <- values.zipWithIndex if v != zero) {
      rv(i) = v
    }
    rv
  }

  private def calculateSize(size: Int): Int = {
    if (size < 4) 4
    else nextPowerOfTwo(size - 1)
  }

  private def nextPowerOfTwo(size: Int): Int = {
    require(size < (1 << 30))
    // http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
    var v = size
    v |= v >> 1
    v |= v >> 2
    v |= v >> 4
    v |= v >> 8
    v |= v >> 16
    v += 1
    v
  }

  private def emptyIndexArray(size: Int) = {
    val arr = new Array[Int](size)
    util.Arrays.fill(arr, -1)
    arr

  }
}
