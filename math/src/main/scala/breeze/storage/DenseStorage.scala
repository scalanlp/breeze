package breeze.storage

/**
 *
 * @author dlwh
 */

trait DenseStorage[@specialized(Int, Double) Elem] extends Storage[Elem] {
  def data: Array[Elem]
  final protected def rawApply(i: Int) = data(i)
  protected def size: Int

  final def activeSize = data.length

  final def valueAt(i: Int) = data(i)

  final def indexAt(i: Int) = i

  protected final def rawUpdate(i: Int, v: Elem) {
    data(i) = v
  }

  protected final def isActive(i: Int) = true
  protected final def allVisitableIndicesActive = true

}
