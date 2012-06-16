package breeze.collection.mutable

/**
 *
 * @author dlwh
 */
trait ArrayLike[T] {
  def apply(i: Int): T
  def update(i: Int, t: T): Unit

  /**
   * Only iterates "active" elements
   */
  def valuesIterator: Iterator[T]


  /**
   * Only iterates "active" keys
   */
  def keysIterator: Iterator[Int]

  def activeSize: Int

  def size: Int

  def length = size

  def foreach[U](f: (T) => U) = valuesIterator foreach f


  /**
   * Only iterates "active" elements
   */
  def iterator = keysIterator zip valuesIterator

  def toArray[U>:T:ClassManifest] = Array.tabulate[U](length)(apply)

  def toList = List.tabulate(length)(apply)

  def toIndexedSeq = List.tabulate(length)(apply)

  def toMap = (keysIterator zip valuesIterator).toMap
}