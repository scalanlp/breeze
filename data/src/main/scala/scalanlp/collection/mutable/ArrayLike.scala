package scalanlp.collection.mutable

/**
 * 
 * @author dlwh
 */
trait ArrayLike[T]  {
  def apply(i: Int):T;

  /**
   * Only iterates "active" elements
   */
  def iterator:Iterator[T];
  def update(i: Int, t: T):Unit

  /**
   * Only iterates "active" keys
   */
  def keysIterator: Iterator[Int];

  def activeSize: Int;
  def size: Int
  def foreach[U](f: (T)=>U) = iterator foreach f;
}