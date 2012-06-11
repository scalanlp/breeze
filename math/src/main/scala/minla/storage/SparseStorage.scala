package minla.storage

/**
 *
 * @author dlwh
 */

trait SparseStorage[@specialized(Int, Double) Elem] extends Storage[Elem] {
  protected implicit def manElem: ClassManifest[Elem]
  def index: Array[Int]
  protected def index_=(arr: Array[Int])

  def data: Array[Elem]
  protected def data_=(arr: Array[Elem])

  protected def default: Elem
  protected def used: Int
  protected def used_=(x: Int)

  protected def rawApply(i : Int): Elem = {
    val offset = findOffset(i)
    if (offset >= 0) data(offset) else default
  }

  def activeSize = data.length

  final protected def valueAt(i: Int) = data(i)

  final protected def indexAt(i: Int) = index(i)

  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the  value is
   * negative and can be converted into an insertion point with ~rv.
   */
  private def findOffset(i : Int) : Int = {
    if (i < 0 || i >= size)
      throw new IndexOutOfBoundsException("Index "+i+" out of bounds [0,"+used+")")

    if (used == 0) {
      // empty list do nothing
      -1
    } else if (i > index(used-1)) {
      // special case for end of list - this is a big win for growing sparse arrays
      ~used
    } else {
      // regular binary search from begin to end (inclusive)
      var begin = 0
      var end = used - 1

      // Simple optimization: position i can't be after offset i.
      if(end > i)
        end = i

      var mid = (end + begin) >> 1
      while (begin <= end) {
        mid = (end + begin) >> 1
        if (index(mid) < i)
          begin = mid + 1
        else if (index(mid) > i)
          end = mid - 1
        else
          mid
      }

      // no match found,  insertion point
      if (i <= index(mid))
        ~mid       // Insert here (before mid)
      else
        ~(mid + 1) // Insert after mid
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
  protected def rawUpdate(i : Int, value : Elem) {
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
        val newIndex = new Array[Int](newLength)
        val newData  = new Array[Elem](newLength)

        // copy existing data into new arrays
        System.arraycopy(index, 0, newIndex, 0, insertPos)
        System.arraycopy(data, 0, newData, 0, insertPos)
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

  protected def isActive(i: Int) = true

  protected def allVisitableIndicesActive = true
}
