package scalanlp.util

import scala.runtime.ScalaRunTime

/**
 * Mixin for case classes that compute their hashcode once, and then cache it.
 * Only good if the case class is immutable, and its contents are also immutable "all the way down".
 */
trait CachedHashCode { this: Product =>
  override lazy val hashCode = ScalaRunTime._hashCode(this);
}
