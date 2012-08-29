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
import breeze.storage.{Storage, DefaultArrayValue, ConfigurableDefault}
import collection.mutable.BitSet
import breeze.util.ArrayUtil
import breeze.generic.{URFunc, UReduceable}
import java.util


/**
 *
 * @author dlwh, dramage
 */
@SerialVersionUID(1L)
final class SparseArray[@specialized(Int, Float, Double) Elem](var index: Array[Int],
                        var data: Array[Elem],
                        private var used: Int,
                        val size: Int,
                        val default: Elem) extends ArrayLike[Elem] with Storage[Elem] with Serializable {



  def this(size: Int, default: Elem)(implicit manElem: ClassManifest[Elem]) = {
    this(Array.empty,Array.empty,0, size, default)
  }

  def this(size: Int)(implicit manElem: ClassManifest[Elem], defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(size, ConfigurableDefault.default[Elem].value(defaultArrayValue))
  }

  @inline
  final def apply(i: Int):Elem = {
    val offset = findOffset(i)
    if (offset >= 0) data(offset) else default
  }

  /**
   * Only iterates "active" elements
   */
  def valuesIterator = data.iterator.take(used)

  /**
   * Only iterates "active" keys
   */
  def keysIterator = index.iterator.take(used)


  def get(i: Int) : Option[Elem] = {
    val offset = findOffset(i)
    if (offset >= 0) Some(data(offset)) else None
  }

  def getOrElse(i : Int, value : =>Elem) : Elem = {
    val offset = findOffset(i)
    if (offset >= 0) data(offset) else value
  }

  def getOrElseUpdate(i : Int, value : =>Elem) : Elem = {
    val offset = findOffset(i)
    if (offset >= 0) data(offset)
    else {
      val v = value
      update(i,v)
      v
    }
  }


  /**
   * Maps all values.  If f(this.default) is not equal to the new default
   * value, the result may be an efficiently dense (or almost dense) paired
   * array.
   */
  def map[B:ClassManifest:DefaultArrayValue](f : Elem=>B) : SparseArray[B] = {
    val newDefault = implicitly[DefaultArrayValue[B]].value
    if (used <= length && f(default) == newDefault) {
      // some default values but f(default) is still default
      val newIndex = new Array[Int](used)
      val newData = new Array[B](used)
      var i = 0
      var o = 0
      while (i < used) {
        newIndex(o) = index(i)
        val newValue = f(data(i))
        if (newValue != newDefault) {
          newData(o) = newValue
          o += 1
        }
        i += 1
      }
      new SparseArray[B](newIndex, newData, o, length, newDefault)
    } else {
      // no default values stored or f(default) is non-default
      val newDefault = f(default)
      val newIndex = new Array[Int](length)
      val newData = new Array[B](length)
      var i = 0
      var o = 0
      while (i < used) {
        while (o < index(i)) {
          newIndex(o) = o
          newData(o) = newDefault
          o += 1
        }
        newIndex(o) = o
        newData(o) = f(data(i))
        o += 1
        i += 1
      }
      while (o < length) {
        newIndex(o) = o
        newData(o) = newDefault
        o += 1
      }
      val rv = new SparseArray[B](newIndex, newData, o, length, newDefault)
      rv.compact()
      rv
    }
  }

  /**
   * Filter's the array by removing all values for which f is false.
   */
  def filter(f : Elem=>Boolean) : SparseArray[Elem] = {
    val newIndex = new Array[Int](used)
    val newData = ArrayUtil.copyOf(data, used)
    var i = 0; var o = 0
    while (i < used) {
      if (f(data(i))) {
        newIndex(o) = index(i) - (i - o)
        newData(o) = data(i)
        o += 1
      }
      i += 1
    }

    if (f(default)) {
      // if default values are accepted, assume we're full length.
      var newLength = length - (i - o)

      // ... and subtract from that length how many defined tail elements
      // were filtered ...
      var ii = used - 1
      while (ii >= 0 && index(ii) > newIndex(o) && index(ii) == newLength - 1) {
        ii -= 1
        newLength -= 1
      }
      new SparseArray[Elem](newIndex, newData, o, newLength, default)
    } else {
      // if default values are not accepted, return a "dense" array by
      // setting each position in newIndex consecutively to forget missing
      // values
      val newLength = o
      new SparseArray[Elem](Array.range(0,newLength), newData.take(newLength), newLength, newLength, default)
    }
  }

  override def toString = iterator.mkString("SparseArray(", ", ", ")")


  def activeSize = used

  final def valueAt(i: Int) = data(i)

  final def indexAt(i: Int) = index(i)

  def contains(i: Int) = findOffset(i) >= 0

  private var lastReturnedPos = -1

  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the  value is
   * negative and can be converted into an insertion point with ~rv.
   */
  protected final def findOffset(i : Int) : Int = {
    if (i < 0 || i >= size)
      throw new IndexOutOfBoundsException("Index "+i+" out of bounds [0,"+used+")")

    if (used == 0) {
      // empty list do nothing
      -1
    } else {
      val index = this.index
      if (i > index(used - 1)) {
        // special case for end of list - this is a big win for growing sparse arrays
        ~used
      } else {
        // regular binary search from begin to end (inclusive)
        var begin = 0
        var end = used - 1

        // Simple optimization: position i can't be after offset i.
        if (end > i)
          end = i

        var found = false

        var mid = (end + begin) >> 1
        // another optimization: cache the last found
        // position and restart binary search from lastReturnedPos
        // This is thread safe because writes of ints
        // are atomic on the JVM.
        // We actually don't want volatile here because
        // we want a poor-man's threadlocal...
        // be sure to make it a local though, so it doesn't
        // change from under us.
        val l = lastReturnedPos
        if (l >= 0 && l < end) {
          mid = l
        }

        // unroll the loop once. We're going to
        // check mid and mid+1, because if we're
        // iterating in order this will give us O(1) access
        val mi = index(mid)
        if (mi == i) {
          found = true
        } else if (mi > i) {
          end = mid - 1
        } else {
          // mi < i
          begin = mid + 1
        }

        // a final stupid check:
        // we're frequently iterating
        // over all elements, so we should check if the next
        // pos is the right one, just to see
        if (!found && mid < end) {
          val mi = index(mid + 1)
          if (mi == i) {
            mid = mid + 1
            found = true
          } else if (mi > i) {
            end = mid
          } else {
            // mi < i
            begin = mid + 2
          }
        }
        if(!found)
          mid = (end + begin) >> 1


        // pick up search.
        while (!found && begin <= end) {
          if (index(mid) < i) {
            begin = mid + 1
            mid = (end + begin) >> 1
          }
          else if (index(mid) > i) {
            end = mid - 1
            mid = (end + begin) >> 1
          }
          else
            found = true
        }

        lastReturnedPos = mid

        if (found || mid < 0)
          mid
        // no match found,  insertion point
        else if (i <= index(mid))
          ~mid // Insert here (before mid)
        else
          ~(mid + 1) // Insert after mid
      }
    }
  }

  /**
   * Sets the given value at the given index if the value is not
   * equal to the current default.  The data and
   * index arrays will be grown to support the insertion if
   * necessary.  The growth schedule doubles the amount
   * of allocated memory at each allocation request up until
   * the sparse array contains 1024 values, at which point
   * the growth is additive: an additional n * 1024 spaces will
   * be allocated for n in 1,2,4,8,16.  The largest amount of
   * space added to this vector will be an additional 16*1024*(sizeof(Elem)+4),
   * which is 196608 bytes at a time for a SparseVector[Double],
   * although more space is needed temporarily while moving to the
   * new arrays.
   */
  @inline
  final def update(i : Int, value : Elem) {
    val offset = findOffset(i)
    if (offset >= 0) {
      // found at offset
      data(offset) = value
    } else if (value != default) {
      // need to insert at ~offset
      val insertPos = ~offset

      used += 1

      if (used > data.length) {
        // need to grow array
        val newLength = {
          if      (data.length == 0)     { 4 }
          else if (data.length < 0x0400) { data.length * 2 }
          else if (data.length < 0x0800) { data.length + 0x0400 }
          else if (data.length < 0x1000) { data.length + 0x0800 }
          else if (data.length < 0x2000) { data.length + 0x1000 }
          else if (data.length < 0x4000) { data.length + 0x2000 }
          else { data.length + 0x4000 }
        }

        // allocate new arrays
        val newIndex = util.Arrays.copyOf(index, newLength)
        val newData  = ArrayUtil.copyOf(data, newLength)

        // copy existing data into new arrays
        System.arraycopy(index, insertPos, newIndex, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data,  insertPos, newData,  insertPos + 1, used - insertPos - 1)

        // update pointers
        index = newIndex
        data = newData
      } else if (used - insertPos > 1) {
        // need to make room for new element mid-array
        System.arraycopy(index, insertPos, index, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data,  insertPos, data,  insertPos + 1, used - insertPos - 1)
      }

      // assign new value
      index(insertPos) = i
      data(insertPos) = value
    }
  }

  def isActive(i: Int) = true

  def allVisitableIndicesActive = true

  /** Compacts the array by removing all stored default values. */
  def compact() {
    val nz = { // number of non-zeros
      var _nz = 0
      var i = 0
      while (i < used) {
        if (data(i) != default) {
          _nz += 1
        }
        i += 1
      }
      _nz
    }

    val newData  = ClassManifest.fromClass(data.getClass.getComponentType).newArray(nz).asInstanceOf[Array[Elem]]
    val newIndex = new Array[Int](nz)

    var i = 0
    var o = 0
    while (i < used) {
      if (data(i) != default) {
        newData(o) = data(i)
        newIndex(o) = index(i)
        o += 1
      }
      i += 1
    }

    data = newData
    index = newIndex
    used = nz
  }


  def use(index: Array[Int], data: Array[Elem], used: Int) {
    this.index = index
    this.data = data
    this.used = used
  }

  def reserve(nnz: Int) {
    if(nnz >= used && nnz != index.length)  {
      index = util.Arrays.copyOf(index, nnz)
      data = ArrayUtil.copyOf(data, nnz)
    }
  }

  /**
   * Like compact, but doesn't look for defaultValues that can be removed.
   */
  def quickCompact() {
    reserve(used)
  }
}

object SparseArray {
  def apply[@specialized(Int, Float, Double) T:ClassManifest:DefaultArrayValue](values : T*) = {
    val rv = new SparseArray[T](Array.range(0, values.length), values.toArray, values.length, values.length, implicitly[DefaultArrayValue[T]].value)
    rv.compact()
    rv
  }

  /**
   * Creates a SparseArray filled with the given value.  The value function
   * is called once initially to test if the returned value is equal to the
   * DefaultArrayValue - if so, an empty SparseArray with initialActiveLength
   * non-zero entries is returned.  Otherwise, an inefficient "dense"
   * SparseArray is returned.
   *
   * @author dramage
   */
  def fill[@specialized(Int, Float, Double) T:ClassManifest:DefaultArrayValue](length : Int)(value : =>T) : SparseArray[T] = {
    if (value != implicitly[DefaultArrayValue[T]].value) {
      val rv = new SparseArray[T](size = length)
      var i = 0
      while (i < length) {
        rv(i) = value
        i += 1
      }
      rv
    } else {
      new SparseArray[T](length)
    }
  }

  def create[@specialized(Int, Float, Double) T:ClassManifest:DefaultArrayValue](length : Int)(values : (Int,T)*) = {
    val rv = new SparseArray[T](length)
    for ((k,v) <- values) {
      rv(k) = v
    }
    rv
  }

  def tabulate[@specialized(Int, Float, Double) T:ClassManifest:DefaultArrayValue](length : Int)(fn : (Int => T)) = {
    val rv = new SparseArray[T](length)
    var i = 0
    while (i < length) {
      val v = fn(i)
      if (v != rv.default) {
        rv(i) = v
      }
      i += 1
    }
    rv.compact()
    rv
  }

  implicit def sparseArrayIsUReduceable[A]:UReduceable[SparseArray[A], A] = {
    new UReduceable[SparseArray[A], A] {
      def apply[Final](c: SparseArray[A], f: URFunc[A, Final]) = {
        f(c.data, c.used)
      }
    }
  }

}