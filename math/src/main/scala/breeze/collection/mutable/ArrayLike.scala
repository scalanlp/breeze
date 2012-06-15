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

  def foreach[U](f: (T) => U) = valuesIterator foreach f


  /**
   * Only iterates "active" elements
   */
  def iterator = keysIterator zip valuesIterator
}