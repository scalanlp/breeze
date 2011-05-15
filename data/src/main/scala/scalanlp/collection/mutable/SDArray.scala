package scalanlp.collection.mutable

/**
 * SemiDense Array: dense storage, but can access keys sparsely
 *
 *
 * @author dlwh
 */
@SerialVersionUID(1)
final class SDArray[@specialized T:ClassManifest](_length: Int, default: =>T)  extends IntMap[T] with Serializable {
  private val arr: Array[T] = Array.fill(_length)(default);
  private val active = collection.mutable.BitSet();

  def apply(i: Int) = arr(i)
  def update(i: Int, x: T) = {
    arr(i) = x;
    active += i;
  }

  def length = arr.length
  def iterator = active.iterator.map { i => (i,arr(i))};
  def keysIterator = active.iterator;
  def activeSize: Int = active.size;

  def foreach[U](f: ((Int,T))=>U) = iterator foreach f;

  def innerArray = arr;

  def toArray[U>:T:ClassManifest] = Array.tabulate[U](arr.length)(i => arr(i));
}