package breeze.collection.mutable

import breeze.storage.{DefaultArrayValue, ConfigurableDefault, SparseStorage}
import collection.mutable.BitSet
import breeze.util.ArrayUtil
import breeze.generic.{URFunc, UReduceable}


/**
 *
 * @author dlwh
 */

class SparseArray[Elem](var index: Array[Int],
                        var data: Array[Elem],
                        protected var used: Int,
                        val size: Int,
                        val default: Elem) extends SparseStorage[Elem] with ArrayLike[Elem] {


  def this(size: Int, default: Elem)(implicit manElem: ClassManifest[Elem]) = {
    this(Array.empty,Array.empty,0, size, default)
  }

  def this(size: Int)(implicit manElem: ClassManifest[Elem], defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(size, ConfigurableDefault.default[Elem].value(defaultArrayValue))
  }

  def apply(x: Int):Elem = rawApply(x)

  def update(i: Int, value: Elem) {
    super.rawUpdate(i, value)
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
}

object SparseArray {
  def apply[@specialized T:ClassManifest:DefaultArrayValue](values : T*) = {
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
  def fill[@specialized T:ClassManifest:DefaultArrayValue](length : Int)(value : =>T) : SparseArray[T] = {
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

  def create[@specialized T:ClassManifest:DefaultArrayValue](length : Int)(values : (Int,T)*) = {
    val rv = new SparseArray[T](length)
    for ((k,v) <- values) {
      rv(k) = v
    }
    rv
  }

  def tabulate[@specialized T:ClassManifest:DefaultArrayValue](length : Int)(fn : (Int => T)) = {
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