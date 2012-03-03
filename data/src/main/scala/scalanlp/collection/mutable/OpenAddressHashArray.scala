package scalanlp.collection.mutable

import collection.mutable.BitSet
import scalala.collection.sparse.DefaultArrayValue
import actors.threadpool.Arrays

trait ConfigurableDefault[V] extends Serializable {
  def value(implicit default: DefaultArrayValue[V]):V
  def fillArray(arr: Array[V], v: V) = arr.asInstanceOf[AnyRef] match {
    case x: Array[Int] => Arrays.fill(arr.asInstanceOf[Array[Int]], v.asInstanceOf[Int])
    case x: Array[Long] => Arrays.fill(arr.asInstanceOf[Array[Long]], v.asInstanceOf[Long])
    case x: Array[Short] => Arrays.fill(arr.asInstanceOf[Array[Short]], v.asInstanceOf[Short])
    case x: Array[Double] => Arrays.fill(arr.asInstanceOf[Array[Double]], v.asInstanceOf[Double])
    case x: Array[Float] => Arrays.fill(arr.asInstanceOf[Array[Float]], v.asInstanceOf[Float])
    case x: Array[Char] => Arrays.fill(arr.asInstanceOf[Array[Char]], v.asInstanceOf[Char])
    case x: Array[Byte] => Arrays.fill(arr.asInstanceOf[Array[Byte]], v.asInstanceOf[Byte])
    case x: Array[_] => Arrays.fill(arr.asInstanceOf[Array[AnyRef]], v.asInstanceOf[AnyRef])
    case _ => throw new RuntimeException("shouldn't be here!")
  }
}

trait LowPriorityConfigurableImplicits {
  implicit def default[V]: ConfigurableDefault[V] = {
    new ConfigurableDefault[V] {
      def value(implicit default: DefaultArrayValue[V]) = default.value
    }
  }

}

object ConfigurableDefault extends LowPriorityConfigurableImplicits {
  implicit def fromV[V](v: V):ConfigurableDefault[V] = {
    new ConfigurableDefault[V] {
      def value(implicit default: DefaultArrayValue[V]) = v
    }
  }
}


/**
 * Represents a sparse int->v assoc sequence.
 * A companion to SparseArray based on hashing. Two parallel arrays, one of ints, one of values.
 * @author dlwh
 */
@SerialVersionUID(1)
class OpenAddressHashArray[@specialized V:ClassManifest:DefaultArrayValue](val size: Int,
                                                                           default: ConfigurableDefault[V] = ConfigurableDefault.default[V],
                                                                           initialSize:Int = 16) extends ArrayLike[V] with Serializable {
  private var index : Array[Int] = new Array[Int](math.max(initialSize,1))
  private var values: Array[V] = new Array[V](math.max(initialSize,1))
  default.fillArray(values, default.value)
  private var occupied = new BitSet()
  private var load = 0;


  def keysIterator = occupied.iterator.map(index)
  def length = size

  def activeSize = load

  def contains(i: Int) = occupied(locate(i))
  def apply(i: Int) = values(locate(i))

  def update(i: Int, v: V) {
    val pos = locate(i)
    if(!occupied(pos) && load * 4 >= index.size * 3) {
      rehash()
      update(i,v)
    } else {
      index(pos) = i
      values(pos) = v
      if(!occupied(pos)) {
        occupied += pos
        load += 1
        if(load * 4 >= index.size * 3) rehash()
      }
    }
  }

  def iterator = occupied.iterator.map(values)

  def pairsIterator = occupied.iterator.map(i => (index(i),values(i)))

  private def locate(i: Int) = {
    if(i >= size) throw new IndexOutOfBoundsException(i + " greater than size of " + size)
    if(i < 0) throw new IndexOutOfBoundsException(i + " less than 0")
    var hash = i.## % index.length
    val start = hash
    var ok = true
    while(occupied(hash) && index(hash) != i) {
      hash += 1
      hash %= index.length
      ok = (hash != start)
      if(!ok) throw new Exception("We ended up with a full hash table somehow. Bad!")
    }
    hash
  }

  private def rehash() {
    val oldOccupied = occupied
    occupied = new BitSet()
    val oldIndex = index
    val oldValues = values
    index = new Array[Int](math.max(oldIndex.size * 2,4))
    values = new Array[V](math.max(oldIndex.size * 2,4))
    default.fillArray(values, default.value)
    load = 0
    for(o <- oldOccupied) {
      update(oldIndex(o),oldValues(o))
    }
  }

  override def toString = {for( o <- occupied iterator) yield (index(o),values(o))}.mkString("OpenAddressHashArray(",",",")");


}