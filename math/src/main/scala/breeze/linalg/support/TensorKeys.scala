package breeze.linalg
package support

/**
 *
 * @author dlwh
 */

class TensorKeys[K, V, +This](private val tensor: This, active: Boolean = false, f: K=>Boolean = { (k: K) => true})(implicit ev: This <:< Tensor[K, V]) {
  def size = tensor.size
  def iterator = {if(active) tensor.activeKeysIterator else tensor.keysIterator}.filter(f)

  def foreach[U](fn: K=>U) = iterator foreach fn
  def filter(p: K=>Boolean): TensorKeys[K, V, This] = withFilter(p)
  def withFilter(p: K=>Boolean): TensorKeys[K, V, This] = new TensorKeys[K, V, This](tensor, active, {(a:K) => f(a) && p(a)})(ev)

  override def toString = iterator.mkString("TensorKeys(",",",")")

  override def equals(p1: Any) = p1 match {
    case x: TensorKeys[_, _, _] => x.eq(this) || iterator.sameElements(x.iterator)
    case _ => false
  }


}
