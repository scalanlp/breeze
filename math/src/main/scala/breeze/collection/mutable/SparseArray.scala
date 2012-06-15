package breeze.collection.mutable

import breeze.storage.{DefaultArrayValue, ConfigurableDefault, SparseStorage}
import collection.mutable.BitSet


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

}
