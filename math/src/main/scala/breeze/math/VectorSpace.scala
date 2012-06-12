package breeze.math

/**
 *
 * @tparam V Vector type
 * @tparam S Scalar type
 * @author dlwh
 */
trait VectorSpace[V, S] {
  def field: Field[S]
  def zero: V
  def timesVS(a: V, b: S):V
  def divVS(a: V, b: S): V

  def plusVV(a: V, b: V):V
  def subVV(a: V, b: V):V


  def close(a: V, b: V, tolerance: Double):Boolean

  // default implementations
  def negate(v: V) = timesVS(v, field.negate(field.one))

}

trait NormedVectorSpace[V, S] extends VectorSpace[V, S] {
  def norm(a: V):S
}

trait InnerProductSpace[V, S] extends NormedVectorSpace[V, S] {
  def dot(a: V, b: V): S
}
