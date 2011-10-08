package scalanlp.tensor.sparse

import scalala.scalar.Scalar
import scalala.tensor.dense.DenseVector
import scalala.tensor.mutable.{VectorColLike, VectorCol}
import java.util.Arrays
import scalala.operators.{OpSub, OpAdd, BinaryOp}
import scalala.generic.collection.{CanMapKeyValuePairs, CanMapValues}

/**
 *
 * More or less taken from old scalala. -- David
 *
 * A sparse vector implementation based on an array of indeces and
 * an array of values.  Inserting a new value takes on the order
 * of the number of non-zeros.  Getting a value takes on the order
 * of the log of the number of non-default values, with special
 * constant time shortcuts for getting the previously accessed
 * element or its successor.  Note that this class is not threadsafe.
 *
 * @author dramage, dlwh
 */
class OldSparseVector(domainSize : Int, var default: Double = 0.0, initialNonzeros : Int = 0) extends VectorCol[Double] with VectorColLike[Double,OldSparseVector] with Serializable {
  if (domainSize < 0)
    throw new IllegalArgumentException("Invalid domain size: "+domainSize);

  val scalar = implicitly[Scalar[Double]];

  /** Data array will be reassigned as the sparse vector grows. */
  private var data : Array[Double] = new Array[Double](initialNonzeros);

  /** Index will be reassigned as the sparse vector grows. */
  private  var index : Array[Int] = new Array[Int](initialNonzeros);

  /** How many iterator of data,index are used. */
  private var used : Int = 0;

  /** The previous index and offset found by apply or update. */
  private var lastOffset = -1;

  /** Use the given index and data arrays, of which the first inUsed are valid. */
  private def use(inIndex : Array[Int], inData : Array[Double], inUsed : Int) = {
    if (inIndex.size != inData.size)
      throw new IllegalArgumentException("Index and data sizes do not match");
    // I spend 7% of my time in this call. It's gotta go.
    //if (inIndex.contains((x:Int) => x < 0 || x > size))
    //  throw new IllegalArgumentException("Index array contains out-of-range index");
    if (inIndex == null || inData == null)
      throw new IllegalArgumentException("Index and data must be non-null");
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
  }

  override def size = domainSize;
  def length = domainSize;

  def activeSize = used;

  def indexAt(offset: Int) = {
    index(offset)
  }

  def valueAt(offset: Int) = {
    data(offset)
  }

  def activeIterator = new Iterator[(Int,Double)] {
    var offset = 0;
    override def hasNext = offset < used;
    override def next() = {
      val rv = (index(offset),data(offset));
      offset += 1;
      rv;
    }
  }

  def activeKeys = index.take(used).iterator;

  def activeValues = data.take(used).iterator;

  /** Zeros this vector, return */
  def zero() = {
    use(new Array[Int](initialNonzeros),
        new Array[Double](initialNonzeros), 0);
  }

  /** Records that the given index was found at this.index(offset). */
  protected final def found(index : Int, offset : Int) : Int = {
    lastOffset = offset;
    offset;
  }

  /**
   * Returns the offset into index and data for the requested vector
   * index.  If the requested index is not found, the return value is
   * negative and can be converted into an insertion point with -(rv+1).
   */
  def findOffset(i : Int) : Int = {
    if (i < 0)
      throw new IndexOutOfBoundsException("index is negative (" + index + ")");
    if (i >= size)
      throw new IndexOutOfBoundsException("index >= size (" + index + " >= " + size + ")");

    val lastIndex = if(lastOffset < 0) -1 else index(lastOffset);

    if (i == lastIndex) {
      // previous element; don't need to update lastOffset
      lastOffset;
    } else if (used == 0) {
      // empty list; do nothing
      -1;
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
        -(mid)-1;     // Insert here (before mid)
      else
        -(mid + 1)-1; // Insert after mid
    }
  }

  override def apply(i : Int) : Double = {
    val offset = findOffset(i);
    if (offset >= 0) data(offset) else default;
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
  def update(i : Int, value : Double) = {
    val offset = findOffset(i);
    if (offset >= 0) {
      // found at offset
      data(offset) = value;
    } else if (value != default) {
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
        newData  = new Array[Double](newLength);
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
    val _default = default;

    val nz = { // number of non-zeros
      var _nz = 0;
      var i = 0;
      while (i < used) {
        if (data(i) != _default) {
          _nz += 1;
        }
        i += 1;
      }
      _nz;
    }

    val newData  = new Array[Double](nz);
    val newIndex = new Array[Int](nz);

    var i = 0;
    var o = 0;
    while (i < used) {
      if (data(i) != _default) {
        newData(o) = data(i);
        newIndex(o) = index(i);
        o += 1;
      }
      i += 1;
    }

    use(newIndex, newData, nz);
  }

  def +=(c: Double):this.type = {
    default += c;
    var offset = 0;
    while(offset < used) {
      data(offset) += c;
      offset += 1;
    }
    this
  }

  def -=(c: Double):this.type = {
    default += c;
    var offset = 0;
    while(offset < used) {
      data(offset) -= c;
      offset += 1;
    }
    this
  }


  def *=(c: Double):this.type = {
    default += c;
    var offset = 0;
    while(offset < used) {
      data(offset) *= c;
      offset += 1;
    }
    this
  }


  def /=(c: Double):this.type = {
    default += c;
    var offset = 0;
    while(offset < used) {
      data(offset) /= c;
      offset += 1;
    }
    this
  }

  /** Optimized implementation for SparseVector dot DenseVector. */
  def dot(that : DenseVector[Double]) : Double = {
    this.checkDomain(that.domain)

    val thisDefault = this.default;
    var sum = 0.0;
    if (thisDefault == 0) {
      var o = 0;
      while (o < this.used) {
        sum += (this.data(o) * that.data(this.index(o)));
        o += 1;
      }
    } else {
      var o1 = 0;
      var i2 = 0;
      while (o1 < this.used) {
        val i1 = this.index(o1);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(i2));
          o1 += 1;
          i2 += 1;
        } else { // i1 < i2
          sum += (thisDefault * that.data(i2));
          i2 += 1;
        }
      }
      // consume remander of that
      while (i2 < that.data.length) {
        sum += (thisDefault * that.data(i2));
        i2 += 1;
      }
    }
    sum;
  }

  /** Optimized implementation for SparseVector dot SparseVector. */
  def dot(that : OldSparseVector) : Double = {
    checkDomain(that.domain);

    var o1 = 0;    // offset into this.data, this.index
    var o2 = 0;    // offset into that.data, that.index
    var sum = 0.0; // the dot product

    val thisDefault = this.default;
    val thatDefault = that.default;

    if (thisDefault == 0 && thatDefault == 0) {
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
        } else if (i1 < i2) {
          o1 += 1;
        } else { // i2 > i1
          o2 += 1;
        }
      }
    } else if (thisDefault == 0) { // && thatDefault != 0
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
        } else if (i1 < i2) {
          sum += (thatDefault * this.data(o1));
          o1 += 1;
        } else { // i2 > i1
          o2 += 1;
        }
      }
      // consume remainder of this
      while (o1 < this.used) {
        sum += (thatDefault * this.data(o1));
        o1 += 1;
      }
    } else if (thatDefault == 0) { // thisDefault != 0
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
        } else if (i1 < i2) {
          o1 += 1;
        } else { // i2 > i1
          sum += (thisDefault * that.data(o2));
          o2 += 1;
        }
      }
      // consume remainder of that
      while (o2 < that.used) {
        sum += (thisDefault * that.data(o2));
        o2 += 1;
      }
    } else { // thisDefault != 0 && thatDefault != 0
      var counted = 0;
      while (o1 < this.used && o2 < that.used) {
        val i1 = this.index(o1);
        val i2 = that.index(o2);
        if (i1 == i2) {
          sum += (this.data(o1) * that.data(o2));
          o1 += 1;
          o2 += 1;
          counted += 1;
        } else if (i1 < i2) {
          sum += (thatDefault * this.data(o1));
          o1 += 1;
          counted += 1;
        } else { // i2 > i1
          sum += (thisDefault * that.data(o2));
          o2 += 1;
          counted += 1;
        }
      }
      // consume remainder of this
      while (o1 < this.used) {
        sum += (thatDefault * this.data(o1));
        o1 += 1;
        counted += 1;
      }
      // consume remainder of that
      while (o2 < that.used) {
        sum += (thisDefault * that.data(o2));
        o2 += 1;
        counted += 1;
      }
      // add in missing product total
      sum += ((size - counted) * (thisDefault * thatDefault));
    }

    sum;
  }

  def copy: OldSparseVector = {
    val r = new OldSparseVector(length,default);
    r.use(Arrays.copyOf(index,index.length),Arrays.copyOf(data,data.length), used);
    r
  }
}

object OldSparseVector {
  implicit val canAddDouble: BinaryOp[OldSparseVector,Double,OpAdd,OldSparseVector] = new BinaryOp[OldSparseVector,Double,OpAdd,OldSparseVector] {
    def opType = OpAdd;

    def apply(v1: OldSparseVector, v2: Double) = {
      v1.copy += v2
    }
  }

  implicit val canSubDouble: BinaryOp[OldSparseVector,Double,OpSub,OldSparseVector] = new BinaryOp[OldSparseVector,Double,OpSub,OldSparseVector] {
    def opType = OpSub;

    def apply(v1: OldSparseVector, v2: Double) = {
      v1.copy -= v2
    }
  }


  implicit val canMapValues: CanMapValues[OldSparseVector,Double,Double,OldSparseVector] = new CanMapValues[OldSparseVector,Double,Double,OldSparseVector] {
    def mapNonZero(from: OldSparseVector, fn: (Double) => Double) = {
      val res = from.copy;
      var offset = 0;
      while(offset < res.activeSize) {
        res(res.indexAt(offset)) = fn(res.valueAt(offset));
        offset += 1;
      }
      if(res.default != 0.0) {
        res.default = fn(res.default);
      }
      res
    }

    def map(from: OldSparseVector, fn: (Double) => Double) = {
      val res = from.copy;
      var offset = 0;
      while(offset < res.activeSize) {
        res(res.indexAt(offset)) = fn(res.valueAt(offset));
        offset += 1;
      }
      res.default = fn(res.default);
      res
    }
  }


  /** Optimized base class for mapping dense columns. */
  implicit val canMapKeyValuePairsSparseVector = new CanMapKeyValuePairs[OldSparseVector,Int,Double,Double,OldSparseVector] {
    def map(from: OldSparseVector, fn: (Int, Double) => Double) = {
      val res = new OldSparseVector(from.size,from.default,from.activeSize)
      for( (k,v) <- res.pairs) {
        res(k) = fn(k,v)
      }
      res
    }

    def mapNonZero(from: OldSparseVector, fn: (Int, Double) => Double) = {
      val res = new OldSparseVector(from.size,from.default,from.activeSize)
      for( (k,v) <- res.pairsIteratorNonZero) {
        res(k) = fn(k,v)
      }
      res
    }
  }

}
