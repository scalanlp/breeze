package breeze.linalg
package support

/**
 *
 * @author dlwh
 */
class TensorValues[K, V, +This](private val tensor: This, active: Boolean = false, f: (V) => Boolean = { (x: Any) => true })(implicit ev: This <:< Tensor[K, V]) {
  def size = tensor.size

  def iterator = {if(active) tensor.activeValuesIterator else tensor.valuesIterator}.filter(f)

  def foreach[U](fn: V => U) = iterator foreach fn

//  def filter(p: V => Boolean) = withFilter(p)
//
//  def withFilter(p: (V) => Boolean): TensorValues[K, V, This] = {
//    new TensorValues[K, V, This](tensor, active, { a => f(a) && p(a) })(ev)
//  }

  override def toString = iterator.mkString("TensorValues(", ",", ")")

  override def equals(p1: Any) = p1 match {
    case x: TensorValues[_, _, _] => x.eq(this) || iterator.sameElements(x.iterator)
    case _ => false
  }

  def map[TT>:This,O,That](fn : (V) => O)
                          (implicit bf : CanMapValues[TT, V, O, That]) : That = {
    tensor.mapValues(fn)(bf.asInstanceOf[CanMapValues[Tensor[K, V], V, O, That]])
  }


 }
