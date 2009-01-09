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

  def internKeys[V](c: scala.collection.Map[T,V]) = {
    Map[T,V]() ++ c.map{ case (k,v) => (intern(k),v)}
  }

  def internValues[K](c: scala.collection.Map[K,T]) = {
    Map[K,T]() ++ c.map{ case (k,v) => (k,intern(v))}
  }

  private val inner = new WeakHashMap[T,WeakReference[T]];
}

object Interner extends Interner[Any] {
  private val typedInterners = new scala.collection.mutable.HashMap[Class[_],Interner[_]] {
    override def default(c: Class[_]) = getOrElseUpdate(c,new Interner[Any]);
  }

  def forClass[T](implicit m:scala.reflect.Manifest[T]):Interner[T] = forClass[T](m.erasure.asInstanceOf[Class[T]]);

  def forClass[T](c: Class[T]) = typedInterners(c).asInstanceOf[Interner[T]];
}
