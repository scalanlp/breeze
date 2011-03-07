package scalanlp.collection.mutable

/**
 * 
 * @author dlwh
 */
trait IntMap[T] {
  def apply(i: Int):T;
  def iterator:Iterator[(Int,T)];
  def update(i: Int, t: T):Unit
  def keysIterator: Iterator[Int];
  def activeSize: Int;
  def size:Int = length;
  def length: Int;
}