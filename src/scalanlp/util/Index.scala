package scalanlp.util;

import scala.collection.mutable._;

/**
 * Class that mimics Java's string indexer, but for anything.
 * Threadsafe.
 *
 * @author dlwh
 */
class Index[T] extends (T=>Int) {
  def apply(t :T) = index(t);
  def unapply(t : Int):Option[T] = get(t);


  def get(t: Int):Option[T] = synchronized {
    if(t > objects.length) None
    else Some(objects(t));
  }

  def index(t : T) = synchronized {
    def nextMax = {
      val m = objects.size;
      objects += t;
      m
    }
    indices.getOrElseUpdate(t,nextMax);
  }


  def clear() = synchronized { indices.clear(); }
  def size = indices.size;

  def indexAll(c : Iterable[T]) = c map apply
  def indexAll(c : Collection[T]) = c map apply
  def indexAll(c : List[T]) = c map apply
  def indexAll(c : Array[T]) = c map apply
  def indexAll(c : Set[T]) = c map apply

  private val objects = new ArrayBuffer[T];
  private val indices = Map[T,Int]();
}

object Index extends Index[Any] {
}
