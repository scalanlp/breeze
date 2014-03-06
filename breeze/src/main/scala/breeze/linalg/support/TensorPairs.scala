package breeze.linalg
package support


/**
 * Class that is kind of like a collection view of the pairs in a tensor.
 * @author dlwh
 */
class TensorPairs[K, V, +This](private val tensor: This, active: Boolean, f: ((K,V)) => Boolean = { (x: (K, V)) => true })(implicit ev: This <:< Tensor[K, V]) {
  def size = tensor.size

  def iterator = {if(active) tensor.activeIterator else tensor.iterator}.filter(f)

  def foreach[U](fn: ((K,V)) => U) = iterator foreach fn
  def foreach[U](fn: (K,V) => U) = iterator foreach { case (a,b) => fn(a,b)}

  def filter(p: ((K,V)) => Boolean) = withFilter(p)

  def withFilter(p: ((K,V)) => Boolean): TensorPairs[K, V, This] = {
    new TensorPairs[K, V, This](tensor, active, { a => f(a) && p(a) })(ev)
  }

  override def toString = iterator.mkString("TensorKeys(", ",", ")")

  override def equals(p1: Any) = p1 match {
    case x: TensorPairs[_, _, _] => x.eq(this) || iterator.sameElements(x.iterator)
    case _ => false
  }

  def map[TT>:This,O,That](fn : ((K,V)) => O)(implicit bf : CanMapKeyValuePairs[TT, K, V, O, That]) : That =
    tensor.mapPairs((k,v) => fn((k,v)))(bf.asInstanceOf[CanMapKeyValuePairs[Tensor[K, V], K, V, O, That]])


}
