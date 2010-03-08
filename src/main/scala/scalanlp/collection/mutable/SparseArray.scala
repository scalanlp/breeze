package scalanlp.collection.mutable;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
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

import scala.collection.generic._;
import scala.reflect.ClassManifest;
import scala.collection.mutable._;

/**
* Treats an array as a sparse array. Logarithm access time.
* 
* @author dlwh
*/
class SparseArray[@specialized T:ClassManifest](val maxSize: Int, initialLength:Int = 3) extends Map[Int,T] with MapLike[Int,T,SparseArray[T]] {
  private var data = new Array[T](initialLength);
  private var index = new Array[Int](initialLength);

  require(data.length == index.length);
 
  override def size = used;
  private var lastIndex = -1;
  private var lastOffset = -1;
  final var used : Int = 0;

  override def empty = new SparseArray[T](0);

  def -=(ind: Int) = {
    val i = findOffset(ind);
    if(i >= 0) {
      System.arraycopy(index, i+1, index, i, used - i - 1);
      System.arraycopy(data,  i+1, data,  i, used - i - 1);
      used -= 1;
    }

    this
  }


  def +=(kv: (Int,T)) = {
    this.update(kv._1,kv._2);
    this
  }

  override def get(x: Int) = {
    val ind = findOffset(x)
    if(ind < 0) None else Some(data(ind));
  }


  final override def foreach[U](f: Function1[(Int,T),U]) {
    var i = 0;
    while(i < used) {
      f(index(i),data(i));
      i += 1;
    }
  }

  override final def iterator = (0 until used).iterator map { i => (index(i),data(i)) };
  override def keysIterator = index.iterator;
  override def valuesIterator = data.iterator;

  // Taken from Scalala

  /** Records that the given index was found at this.index(offset). */
  final private def found(index : Int, offset : Int) : Int = {
    lastOffset = offset;
    lastIndex = index;
    return offset;
  }

    /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the return value is
   * negative and can be converted into an insertion point with -(rv+1).
   */
  private def findOffset(i : Int) : Int = {
    if (i < 0)
      throw new IndexOutOfBoundsException("index is negative (" + index + ")");
    if (i >= maxSize)
      throw new IndexOutOfBoundsException("index >= maxSize (" + index + " >= " + maxSize + ")");

    if (i == lastIndex) {
      // previous element; don't need to update lastOffset
      return lastOffset;
    } else if (used == 0) {
      // empty list; do nothing
      return -1;
    } else {
      // regular binary search
      var begin = 0;
      var end = used - 1;

      // narrow the search if we have a previous reference
      if (lastIndex >= 0 && lastOffset >= 0) {
        if (i < lastIndex) {
          // in range preceding last request
          end = lastOffset;
        } else {
          // in range following last request
          begin = lastOffset;

          if (begin + 1 <= end && index(begin + 1) == i) {
            // special case: successor of last request
            return found(i, begin + 1);
          }
        }
      }

      // Simple optimization:
      // the i'th entry can't be after entry i.
      if(end > i)
        end = i;

      // this assert is for debug only
      //assert(begin >= 0 && end >= begin,
      //       "Invalid range: "+begin+" to "+end);

      var mid = (end + begin) >> 1;

      while (begin <= end) {
        mid = (end + begin) >> 1;
        if (index(mid) < i)
          begin = mid + 1;
        else if (index(mid) > i)
          end = mid - 1;
        else
          return found(i, mid);
      }

      // no match found, return insertion point
      if (i <= index(mid))
        return -(mid)-1;     // Insert here (before mid)
      else
        return -(mid + 1)-1; // Insert after mid
    }
  }

  override def apply(i : Int) : T = {
    val offset = findOffset(i);
    if (offset >= 0) data(offset) else default(i);
  }

  /**
   * Sets the given value at the given index if the value is not
   * equal to the current default.  The data and
   * index arrays will be grown to support the insertion if
   * necessary.  The growth schedule doubles the amount
   * of allocated memory at each allocation request up until
   * the sparse vector contains 1024 iterator, at which point
   * the growth is additive: an additional n * 1024 spaces will
   * be allocated for n in 1,2,4,8,16.  The largest amount of
   * space added to this vector will be an additional 16*1024*(8+4) =
   * 196608 bytes, although more space is needed temporarily
   * while moving to the new arrays.
   */
  override def update(i : Int, value : T) = {
    val offset = findOffset(i);
    if (offset >= 0) {
      // found at offset
      data(offset) = value;
    } else if (value != null) {
      // need to insert at position -(offset+1)
      val insertPos = -(offset+1);

      used += 1;

      var newIndex = index;
      var newData = data;

      if (used > data.length) {
        val newLength = {
          if (data.length < 8) { 8 }
          else if (data.length > 16*1024) { data.length + 16*1024 }
          else if (data.length > 8*1024)  { data.length +  8*1024 }
          else if (data.length > 4*1024)  { data.length +  4*1024 }
          else if (data.length > 2*1024)  { data.length +  2*1024 }
          else if (data.length > 1*1024)  { data.length +  1*1024 }
          else { data.length * 2 }
        }

        // copy existing data into new arrays
        newIndex = new Array[Int](newLength);
        newData  = new Array[T](newLength);
        System.arraycopy(index, 0, newIndex, 0, insertPos);
        System.arraycopy(data, 0, newData, 0, insertPos);
      }

      // make room for insertion
      System.arraycopy(index, insertPos, newIndex, insertPos + 1, used - insertPos - 1);
      System.arraycopy(data,  insertPos, newData,  insertPos + 1, used - insertPos - 1);

      // assign new value
      newIndex(insertPos) = i;
      newData(insertPos) = value;

      // record the insertion point
      found(i,insertPos);

      // update pointers
      index = newIndex;
      data = newData;
    }
  }

  /** Compacts the vector by removing all stored default values. */
  def compact() {

    val nz = { // number of non-zeros
      var _nz = 0;
      var i = 0;
      while (i < used) {
        if (data(i) != null) {
          _nz += 1;
        }
        i += 1;
      }
      _nz;
    }

    val newData  = new Array[T](nz);
    val newIndex = new Array[Int](nz);

    var i = 0;
    var o = 0;
    while (i < used) {
      if (data(i) != null) {
        newData(o) = data(i);
        newIndex(o) = index(i);
        o += 1;
      }
      i += 1;
    }

    use(newIndex, newData, nz);
  }


  /** Use the given index and data arrays, of which the first inUsed are valid. */
  private def use(inIndex : Array[Int], inData : Array[T], inUsed : Int) = {
    if (inIndex == null || inData == null)
      throw new IllegalArgumentException("Index and data must be non-null");
    if (inIndex.size != inData.size)
      throw new IllegalArgumentException("Index and data sizes do not match");
    // I spend 7% of my time in this call. It's gotta go.
    //if (inIndex.contains((x:Int) => x < 0 || x > size))
    //  throw new IllegalArgumentException("Index array contains out-of-range index");
    if (inIndex.size < inUsed)
      throw new IllegalArgumentException("Used is greater than provided array");
    // I spend 7% of my time in this call. It's gotta go. and this one.
    //for (i <- 1 until used; if (inIndex(i-1) > inIndex(i))) {
    //  throw new IllegalArgumentException("Input index is not sorted at "+i);
    //}
    //for (i <- 0 until used; if (inIndex(i) < 0)) {
    //  throw new IllegalArgumentException("Input index is less than 0 at "+i);
    //}

    data = inData;
    index = inIndex;
    used = inUsed;
    lastOffset = -1;
    lastIndex = -1;
  }



}