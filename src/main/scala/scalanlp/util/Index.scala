package scalanlp.util;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


import scala.collection.mutable._;
import scalala.collection._;

/**
 * Trait that builds a 1-to-1 mapping between Ints and T's, which
 * is very useful for efficiency concerns.
 * 
 * Two extra views are provided: the index.synchronized view
 * enables threadsafe access and the index.immutable view keeps
 * prevents the (view) from being updated. 
 *
 * See the companion object for easy ways to create these.
 *
 * @author dlwh, dramage
 */
trait Index[T] extends Injection[T,Int] with Collection[T] {
  override def apply(t : T) = index(t);

  def contains(t:T): Boolean;

  /**
  * Returns Some(t) if this int corresponds to some object,
  * and None otherwise.
  */
  def unapply(pos : Int):Option[T];

  /**
   * Returns an object at the given position or throws an exception if it's
   * not found.
   */
  def get(pos : Int) : T = unapply(pos).getOrElse(throw new IndexOutOfBoundsException());

  /**
   * Returns an integer index for the given object.
   */
  def index(t : T): Int;

  /**
  * Returns Some(i) if the object has been indexed, or None.
  */
  def indexOpt(t:T): Option[Int];
  
  /** Override indexOf's slow, deprecated behavior. */
  override def indexOf[B >: T](elem: B): Int = index(elem.asInstanceOf[T]);

  //
  // these accessors are kept separate to preserve collection subtype
  //
  def indexAll(c : Iterator[T]) = c map apply;
  def indexAll(c : Iterable[T]) = c map apply;
  def indexAll(c : Collection[T]) = c map apply;
  def indexAll(c : List[T]) = c map apply;
  def indexAll(c : Array[T]) = c map apply;
  def indexAll(c : Set[T]) = c map apply;
  def indexAll(c : Seq[T]) = c map apply;

  def indexKeys[V](c: scala.collection.Map[T,V]) = {
    Map[T,V]() ++ c.map{ case (a,b) => (this(a),b)}
  }
  
  def indexValues[K](c: scala.collection.Map[K,T]) = {
    Map[K,T]() ++ c.map{ case (a,b) => (a,this(b))}
  }

  def getAll(c : Iterator[Int]) = c map get;
  def getAll(c : Iterable[Int]) = c map get;
  def getAll(c : Collection[Int]) = c map get;
  def getAll(c : List[Int]) = c map get;
  def getAll(c : Array[Int]) = c map get;
  def getAll(c : Set[Int]) = c map get;

  /** Returns a synchronized view of the index. */
  def synchronized : Index[T] = {
    val outer = this;
    new Index[T] {
      override def elements = outer.elements;
      override def size = outer.size;
      override def contains(t:T) = outer synchronized{ outer.contains(t); }
      override def unapply(pos : Int) = outer synchronized { outer.unapply(pos); }
      override def index(t : T) = outer synchronized { outer.index(t); }
      override def indexOpt(t : T) = outer synchronized { outer.indexOpt(t); }
    }
  }
}

/**
 * Class that builds a 1-to-1 mapping between Ints and T's, which
 * is very useful for efficiency concerns.
 * 
 * Two extra views are provided: the index.synchronized view
 * enables threadsafe access and the index.immutable view keeps
 * prevents the (view) from being updated. 
 *
 * @author dlwh, dramage
 */
@serializable private class HashIndex[T] extends Index[T] {
  /** Forward map from int to object */
  private val objects = new ArrayBuffer[T];
  
  /** Map from object back to int index */
  private val indices = Map[T,Int]();
  
  override def elements = objects.elements;
  
  override def size = indices.size;

  def contains(t:T) = indices contains t;

  /**
  * Returns the integer corresponding to T, or creates a new one if it's not there.
  */
  def index(t: T) = {
    def nextMax = {
      val m = objects.size;
      objects += t;
      m
    }
    indices.getOrElseUpdate(t,nextMax);
  }

  def unapply(pos : Int):Option[T] = {
    if(pos >= 0 && pos < objects.length)
      Some(objects(pos))
    else None
  }

  /**
  * Returns Some(i) if the object has been indexed, or None
  */
  def indexOpt(t:T): Option[Int] = {
    indices.get(t);
  }

}

/**
* For use when we need an index, but we already have (densely packed) positive
* ints and don't want hash overhead.
*
* @author dlwh
*/
class DenseIntIndex extends Index[Int] {
  def size = maxSeen;
  private var maxSeen = 0;
  override def contains(t:Int)= t < maxSeen && t >= 0;

  def index(t: Int) = {
    maxSeen = maxSeen max t;
    t
  }

  def indexOpt(t: Int) = {
    maxSeen = maxSeen max t;
    Some(t)
  }

  def unapply(t: Int) = Some(t);

  def elements = (0 to maxSeen).elements;
}

trait Indexed[T] {
  val index: Index[T] = Index[T]();
}

trait SynchronouslyIndexed[T] extends Indexed[T] {
  override val index: Index[T] = new HashIndex[T].synchronized;
}

/**
 * Utilities for manipulating and creating Index objects.
 */
object Index {
  /** Constructs an empty index. */
  def apply[T]() : Index[T] = new HashIndex[T];
  
  /** Constructs an Index from some elements. */
  def apply[T](elements : Iterator[T]) : Index[T] = {
    val index = Index[T]();
    // read through all elements now -- don't lazily defer evaluation
    for (element <- elements) {
      index.index(element);
    }
    return index;
  }
  
  /** Constructs an Index from some elements. */
  def apply[T](iterable : Iterable[T]) : Index[T] = {
    val index = Index[T]();
    // read through all elements now -- don't lazily defer evaluation
    for (element <- iterable) {
      index.index(element);
    }
    return index;
  }
  
  /**
   * Loads a String index, one line per item with line
   * numbers (starting at 0) as the indices.
   */
  def load(source : {def getLines : Iterator[String]}) : Index[String] = {
    apply(source.getLines.map(_.stripLineEnd));
  }
}
