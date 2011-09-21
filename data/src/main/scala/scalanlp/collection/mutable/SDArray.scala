package scalanlp.collection.mutable

/**
 * SemiDense Array: dense storage, but can iterate keys sparsely
 *
 *
 * @author dlwh
 */
@SerialVersionUID(1)
final class SDArray[@specialized T:ClassManifest](_length: Int, default: =>T)  extends ArrayLike[T] with Serializable {
  private val arr: Array[T] = Array.fill(_length)(default);
  private val active = collection.mutable.BitSet();

  override def apply(i: Int) = arr(i)
  override def update(i: Int, x: T) = {
    arr(i) = x;
    active += i;
    this
  }

  def length = arr.length
  def iterator = active.iterator.map { arr }
  override def keysIterator = active.iterator;
  def activeSize: Int = active.size;

  def innerArray = arr;

  override def size =arr.length
}