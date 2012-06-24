package breeze.collection.mutable

import collection.mutable.BitSet
import breeze.storage.{Storage, ConfigurableDefault, DefaultArrayValue}
import java.util


/**
 * This is a Sparse Array implementation backed by a linear-probing
 * open address hash table.
 *
 * @author dlwh
 */
final class OpenAddressHashArray[@specialized Elem] private (protected var _index: Array[Int],
                                protected var _data: Array[Elem],
                                 protected var load: Int,
                                 val size: Int,
                                 val default: ConfigurableDefault[Elem] = ConfigurableDefault.default[Elem])
                                (implicit protected val manElem: ClassManifest[Elem],
                                 protected val defaultArrayValue: DefaultArrayValue[Elem]) extends Storage[Elem] with ArrayLike[Elem] {
  require(size > 0, "Size must be positive, but got " + size)

  def this(size: Int, default: ConfigurableDefault[Elem],
           initialSize: Int)
          (implicit manElem: ClassManifest[Elem],
           defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(OpenAddressHashArray.emptyIndexArray(OpenAddressHashArray.calculateSize(initialSize)),
      default.makeArray(OpenAddressHashArray.calculateSize(initialSize)),
      0,
      size,
      default)
  }

  def this(size: Int,
           default: ConfigurableDefault[Elem])
          (implicit manElem: ClassManifest[Elem],
           defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(size, default, 16)
  }

  def this(size: Int)(implicit manElem: ClassManifest[Elem], defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(size, ConfigurableDefault.default[Elem])
  }

  def data = _data
  def index = _index

  /**
   * Only iterates "active" elements
   */
  def valuesIterator = activeValuesIterator

  def valueAt(i: Int) = data(i)
  def indexAt(i: Int) = index(i)

  def keysIterator = index.iterator.filter(_ >= 0)

  def activeSize = load

  def contains(i: Int) = index(locate(i)) >= 0

  def isActive(i: Int) = contains(i)

  def allVisitableIndicesActive = false

  final def apply(i: Int) = {
    if(i < 0 || i >= size) throw new IndexOutOfBoundsException()
    if(index.length == 0) default.value
    else data(locate(i))
  }

  final def update(i: Int, v: Elem) {
    if(i < 0 || i >= size) throw new IndexOutOfBoundsException()
    val pos = locate(i)
    _data(pos) = v
    if(_index(pos) != i) {
      load += 1
      if(load * 3 > _index.length * 2) {
        rehash()
        update(i, v)
      } else {
        _index(pos) = i
      }
    }
  }

  def activeKeysIterator = keysIterator
  def activeValuesIterator = activeIterator.map(_._2)
  def activeIterator = (index.iterator zip data.iterator).filter(_._1 >= 0)

  private def locate(i: Int) = {
    if(i >= size) throw new IndexOutOfBoundsException(i + " greater than size of " + size)
    if(i < 0) throw new IndexOutOfBoundsException(i + " less than 0")
    val index = this.index
    val len = index.length
    val lenm1 = len - 1
    var hash = i.## & lenm1
    var numProbes = 0
    while(index(hash) != i && index(hash) >= 0) {
      numProbes += 1
      hash += numProbes
      hash &= (lenm1)
    }
    hash
  }

  final protected def rehash() {
    val oldIndex = index
    val oldValues = data
    val newSize = OpenAddressHashArray.calculateSize(oldIndex.size)
    _index = new Array[Int](newSize)
    util.Arrays.fill(_index, -1)
    _data = new Array[Elem](newSize)
    default.fillArray(_data, default.value)
    load = 0
    var i = 0
    while(i < oldIndex.length) {
      if(oldIndex(i) >= 0) {
        update(oldIndex(i),oldValues(i))
      }
      i += 1
    }
  }

  /**
   * How many elements must be iterated over using valueAt/indexAt.
   * @return
   */
  override def iterableSize = index.length

  override def toString: String = activeIterator.mkString("OpenAddressHashArray(",", ", ")")
}

object OpenAddressHashArray {
  def apply[@specialized T:ClassManifest:DefaultArrayValue](values : T*) = {
    val rv = new OpenAddressHashArray[T](values.length)
    val default = implicitly[DefaultArrayValue[T]].value
    for( (v,i) <- values.zipWithIndex if v != default) {
      rv(i) = v
    }
    rv
  }

  private def calculateSize(size: Int): Int = {
    if(size < 4) 4
    else nextPowerOfTwo(size)
  }

  private def nextPowerOfTwo(size: Int): Int = {
    require(size < (1<<30) )
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
