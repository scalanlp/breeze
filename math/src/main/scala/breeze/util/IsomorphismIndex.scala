package breeze.util

/**
 * Maps an index of type T to an index of type U using an isomorphism implicit
 * @author dlwh
 */
@SerialVersionUID(1)
class IsomorphismIndex[T, U](val innerIndex: Index[T])(implicit iso: Isomorphism[T, U])
    extends Index[U]
    with Serializable {
  def apply(u: U): Int = innerIndex(iso.backward(u))

  /**
   * Returns Some(t) if this int corresponds to some object,
   * and None otherwise.
   */
  def unapply(i: Int): Option[U] = innerIndex.unapply(i).map(iso.forward)

  /** Returns the indexed items along with their indicies */
  def pairs: Iterator[(U, Int)] = innerIndex.pairs.map(pair => (iso.forward(pair._1), pair._2))

  def iterator: Iterator[U] = innerIndex.iterator.map(iso.forward)

  override def size: Int = innerIndex.size

  override def contains(u: U): Boolean = innerIndex.contains(iso.backward(u))

  override def get(i: Int): U = iso.forward(innerIndex.get(i))
}
