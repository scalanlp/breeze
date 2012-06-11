package minla.storage

/**
 * Interface for underlying storage used by Vectors
 * @author dlwh
 */
trait Storage[@specialized(Int,Double,Float) Elem] {
  /**
   * Returns the actual flat array of elements used.
   * @return
   */
  def data: Array[Elem]

  /**
   *
   * @param i the logical index to look up
   * @return
   */
  protected def rawApply(i: Int): Elem
  protected def rawUpdate(i: Int, v: Elem)

  /**
   * How many elements are logically stored here. This may be <= activeSize.
   * @return
   */
  protected def size: Int

  /**
   * How many elements are stored in terms of space.
   *
   * @return
   */
  def activeSize: Int

  /**
   * same as data(i). Gives the value at the underlying offset.
   * @param i index into the data array
   * @return
   */
  protected def valueAt(i: Int): Elem

  /**
   * Gives the logical index from the physical index.
   * @param i
   * @return
   */
  protected def indexAt(i: Int): Int

  /**
   * Some arrays
   * @param i
   * @return
   */
  protected def isActive(i: Int):Boolean

  /**
   * Only gives true if isActive would return true for all i. (May be callde anyway)
   * @return
   */
  protected def allVisitableIndicesActive:Boolean
}
