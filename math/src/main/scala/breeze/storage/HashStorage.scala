package breeze.storage

import collection.mutable.BitSet
import java.util.Arrays


/**
 * Represents a sparse int->v assoc sequence.
 * A companion to SparseArray based on hashing. Two parallel arrays, one of ints, one of data.
 * @author dlwh
 */
@SerialVersionUID(1)
trait HashStorage[@specialized(Int, Double) V]  extends Storage[V] {
  implicit protected def manElem: ClassManifest[V]
  implicit protected def defaultArrayValue: DefaultArrayValue[V]

  def index : Array[Int]
  protected def index_=(index : Array[Int])
  def data: Array[V]
  protected def data_=(data: Array[V])
  protected def default: ConfigurableDefault[V]

  protected def occupied:BitSet
  protected def occupied_=(oc:BitSet)
  protected def load: Int
  protected def load_=(load: Int)

  def valueAt(i: Int) = data(i)
  def indexAt(i: Int) = index(i)

  def keysIterator = occupied.iterator.map(index)
  def length = size

  def activeSize = load

  def contains(i: Int) = occupied(locate(i))

  def isActive(i: Int) = contains(i)

  def allVisitableIndicesActive = false

  protected def rawApply(i: Int) = data(locate(i))

  protected def rawUpdate(i: Int, v: V) {
    val pos = locate(i)
    if(!occupied.contains(pos) && load * 4 >= index.size * 3) {
      rehash()
      rawUpdate(i,v)
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
    var hash = i.## % index.length
    val len = index.length
    while(index(hash) != i && occupied.contains(hash)) {
      hash += 1
      hash %= len
    }
    hash
  }

  private def rehash() {
    val oldOccupied = occupied
    occupied = new BitSet()
    val oldIndex = index
    val oldValues = data
    index = new Array[Int](math.max(oldIndex.size * 2,4))
    data = new Array[V](math.max(oldIndex.size * 2,4))
    default.fillArray(data, default.value)
    load = 0
    for(o <- oldOccupied) {
      rawUpdate(oldIndex(o),oldValues(o))
    }
  }

  /**
   * How many elements must be iterated over using valueAt/indexAt.
   * @return
   */
  override def iterableSize = index.length
}

