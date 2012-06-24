package breeze.collection.mutable

import collection.mutable.BitSet
import breeze.storage.{Storage, ConfigurableDefault, DefaultArrayValue}


/**
 *
 * @author dlwh
 */

final class OpenAddressHashArray[@specialized Elem](var index: Array[Int],
                                 var data: Array[Elem],
                                 protected var occupied: BitSet,
                                 protected var load: Int,
                                 val size: Int,
                                 val default: ConfigurableDefault[Elem] = ConfigurableDefault.default[Elem])
                                (implicit protected val manElem: Manifest[Elem],
                                 protected val defaultArrayValue: DefaultArrayValue[Elem]) extends Storage[Elem] with ArrayLike[Elem] {


  def this(size: Int, default: ConfigurableDefault[Elem], initialSize: Int)(implicit manElem: Manifest[Elem], defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(new Array[Int](initialSize),default.makeArray(initialSize), BitSet.empty,0, size, default)
  }

  def this(size: Int, default: ConfigurableDefault[Elem])(implicit manElem: Manifest[Elem], defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(size, default, 16)
  }

  def this(size: Int)(implicit manElem: Manifest[Elem], defaultArrayValue: DefaultArrayValue[Elem]) = {
    this(size, ConfigurableDefault.default[Elem])
  }

  /**
   * Only iterates "active" elements
   */
  def valuesIterator = activeValuesIterator

  def valueAt(i: Int) = data(i)
  def indexAt(i: Int) = index(i)

  def keysIterator = occupied.iterator.map(index)

  def activeSize = load

  def contains(i: Int) = occupied(locate(i))

  def isActive(i: Int) = contains(i)

  def allVisitableIndicesActive = false

  final def apply(i: Int) = {
    if(index.length == 0) default.value
    else data(locate(i))
  }

  final def update(i: Int, v: Elem) {
    if(index.length == 0) {
      rehash()
    }
    val pos = locate(i)
    if(!occupied.contains(pos) && load * 4 >= index.size * 3) {
      rehash()
      update(i,v)
    } else {
      index(pos) = i
      data(pos) = v
      if(!occupied.contains(pos)) {
        occupied += pos
        load += 1
        if(load * 4 >= index.size * 3) rehash()
      }
    }
  }

  def activeKeysIterator = occupied.iterator
  def activeValuesIterator = occupied.iterator.map(data)
  def activeIterator = occupied.iterator.map(i => (index(i),data(i)))

  private def locate(i: Int) = {
    if(i >= size) throw new IndexOutOfBoundsException(i + " greater than size of " + size)
    if(i < 0) throw new IndexOutOfBoundsException(i + " less than 0")
    val index = this.index
    var hash = i.## % index.length
    val len = index.length
    val occupied = this.occupied
    while(index(hash) != i && occupied.contains(hash)) {
      hash += 1
      hash %= len
    }
    hash
  }

  final protected def rehash() {
    val oldOccupied = occupied
    occupied = new BitSet()
    val oldIndex = index
    val oldValues = data
    index = new Array[Int](math.max(oldIndex.size * 2,4))
    data = new Array[Elem](math.max(oldIndex.size * 2,4))
    default.fillArray(data, default.value)
    load = 0
    for(o <- oldOccupied) {
      update(oldIndex(o),oldValues(o))
    }
  }

  /**
   * How many elements must be iterated over using valueAt/indexAt.
   * @return
   */
  override def iterableSize = index.length

}
