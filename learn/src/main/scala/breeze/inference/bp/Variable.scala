package breeze.inference.bp

import breeze.util.{DenseIntIndex, Index}
import actors.threadpool.AtomicInteger

/**
 * A Variable has a domain and an optional name. Domains are indices.
 *
 * At some point this will become DiscreteVariable, but not now.
 *
 *
 * @param name the name
 * @param domain index of valid assignments
 * @tparam T the underlying type
 */
case class Variable[T](name: String, domain: Index[T]) {
  def size = domain.size
}

object Variable {
  /**
   * Makes a variable with a default name.
   * @param index
   * @tparam T
   * @return
   */
  def apply[T](index: Index[T]):Variable[T] = new Variable(nextName(), index)

  /**
   * Makes a variable with a default name and a [[breeze.util.DenseIntIndex]] domain.
   * @param range step size must be 1.
   * @tparam T
   * @return
   */
  def apply[T](range: Range):Variable[Int] = {
    require(range.step == 1, "Step must be 1")
    apply(new DenseIntIndex(range.head, range.last + 1))
  }


  private val nextVariableId = new AtomicInteger(0)
  private def nextName() = "V" + nextVariableId.incrementAndGet()
}
