package breeze.linalg
package support

/**
 *
 * @author dlwh
 */
class TensorActive[K, V, +This](private val tensor: This)(implicit ev: This <:< Tensor[K, V])  {
  def iterator = tensor.activeIterator
  def keys = new TensorKeys[K, V, This](tensor, true)(ev)
  def values = new TensorValues[K, V, This](tensor, true)(ev)
  def pairs = new TensorPairs[K, V, This](tensor, true)(ev)
}
