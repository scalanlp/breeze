package scalanlp.util;

import scala.collection.jcl.WeakHashMap;
import java.lang.ref.WeakReference;

/**
 * Class that mimics Java's string interner, but for anything.
 * Threadsafe.
 *
 * @author dlwh
 */
class Interner[T] extends (T=>T) {
  override def apply(t :T) = intern(t);

  def intern(t : T):T = synchronized {
    inner.getOrElseUpdate(t,new WeakReference[T](t)).get;
  }

  def clear() = inner.clear();
  def size = inner.size;

  def internAll(c : Iterable[T]) = c map apply
  def internAll(c : Collection[T]) = c map apply
  def internAll(c : List[T]) = c map apply
  def internAll(c : Array[T]) = c map apply
  def internAll(c : Set[T]) = c map apply

  private val inner = new WeakHashMap[T,WeakReference[T]];
}

object Interner extends Interner[Any] {
}
