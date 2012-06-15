package breeze.collection.mutable

import breeze.storage.SparseStorage

/**
 *
 * @author dlwh
 */

class SparseArray[Elem](var index: Array[Int],
                        var data: Array[Elem],
                        protected var used: Int,
                        val size: Int,
                        val default: Elem) extends SparseStorage[Elem] with ArrayLike[Elem] {

  def apply(x: Int):Elem = rawApply(x)

  def update(i: Int, value: Elem) {
    super.rawUpdate(i, value)
  }

  /**
   * Only iterates "active" elements
   */
  def iterator = data.iterator.take(used)

  /**
   * Only iterates "active" keys
   */
  def keysIterator = index.iterator.take(used)

}
