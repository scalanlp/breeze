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

/**
 * Class that mimics Java's string indexer, but for anything.
 * 
 * Two extra views are provided: the index.synchronized view
 * enables threadsafe access and the index.immutable view keeps
 * prevents the (view) from being updated. 
 *
 * @author dlwh, dramage
 */
@serializable
@SerialVersionUID(-447184846322506350l)
class Index[T] extends (T=>Int) with Collection[T] {
  /** Forward map from int to object */
  private val objects = new ArrayBuffer[T];
  
  /** Map from object back to int index */
  private val indices = Map[T,Int]();
  
  override def apply(t : T) = index(t);
  def unapply(pos : Int):T = get(pos);

  override def elements = objects.elements;
  
  override def size = indices.size;

  /**
   * Returns an object at the given position or throws an exception if it's
   * not found.
   */
  def get(pos : Int) : T = {
    if (pos < 0 || pos >= objects.length) throw new IndexOutOfBoundsException("Index "+pos+" is out of range");
    else objects(pos);
  }

  /**
   * Returns an integer index for the given object.  By default,
   * this method will allocate a new index (at the end) if the
   * object was not found, but an immutable view may return -1
   * for missing objects.  
   */
  def index(t : T) = {
    def nextMax = {
      val m = objects.size;
      objects += t;
      m
    }
    indices.getOrElseUpdate(t,nextMax);
  }
  
  /** Override indexOf's slow, deprecated behavior. */
  override def indexOf[B >: T](elem: B): Int = index(elem.asInstanceOf[T]);
  
  /**
   * Clears the index.
   */
  def clear() = {
    indices.clear();
    objects.clear();
  }

  //
  // these accessors are kept separate to preserve collection subtype
  //
  
  def indexAll(c : Iterator[T]) = c map apply;
  def indexAll(c : Iterable[T]) = c map apply;
  def indexAll(c : Collection[T]) = c map apply;
  def indexAll(c : List[T]) = c map apply;
  def indexAll(c : Array[T]) = c map apply;
  def indexAll(c : Set[T]) = c map apply;

  def indexKeys[V](c: scala.collection.Map[T,V]) = {
    Map[T,V]() ++ c.map{ case (a,b) => (this(a),b)}
  }
  
  def indexValues[K](c: scala.collection.Map[K,T]) = {
    Map[K,T]() ++ c.map{ case (a,b) => (a,this(b))}
  }

  def getAll(c : Iterator[Int]) = c map unapply;
  def getAll(c : Iterable[Int]) = c map unapply;
  def getAll(c : Collection[Int]) = c map unapply;
  def getAll(c : List[Int]) = c map unapply;
  def getAll(c : Array[Int]) = c map unapply;
  def getAll(c : Set[Int]) = c map unapply;

  //
  // Index views.
  //

  /** Returns an immutable view of the index. */
  def immutable : Index[T] = {
    val outer = this;
    new Index[T] {
      override def elements = outer.elements;
      override def size = outer.size;
      override def get(pos : Int) = outer.get(pos);
      override def index(t : T) = outer.indices.getOrElse(t,-1);
      override def clear = {};
    }
  }
  
  /** Returns a synchronized view of the index. */
  def synchronized : Index[T] = {
    val outer = this;
    new Index[T] {
      override def elements = outer.elements;
      override def size = outer.size;
      override def get(pos : Int) = synchronized { outer.get(pos); }
      override def index(t : T) = synchronized { outer.index(t); }
      override def clear = synchronized { outer.clear; }
    }
  }
}

/**
 * Utilities for manipulating and creating Index objects.
 */
object Index extends Index[Any] {
  /** Constructs an empty index. */
  def apply[T]() : Index[T] = new Index[T];
  
  /** Constructs an Index from some elements. */
  def apply[T](elements : Iterator[T]) : Index[T] = {
    val index = new Index[T];
    // read through all elements now -- don't lazily defer evaluation
    for (element <- elements) {
      index.index(element);
    }
    return index;
  }
  
  /** Constructs an Index from some elements. */
  def apply[T](iterable : Iterable[T]) : Index[T] = {
    val index = new Index[T];
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
