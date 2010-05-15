package scalanlp.concurrent

/**
 * A more Scala-like ThreadLocal. Takes a default value that is created per thread.
 */
class ThreadLocal[T](default: =>T) extends java.lang.ThreadLocal[T] with Function0[T] {
  override protected def initialValue: T = default;
  def apply() = get();
}
