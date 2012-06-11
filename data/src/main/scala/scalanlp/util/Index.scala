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

import scala.collection.IterableProxy;
import scala.collection.mutable.{ArrayBuffer,HashMap}
import java.util.Arrays
;

/**
 * Trait that marks an O(1) bidirection map between Ints (increasing from 0)
 * and T's.  This class is used, for example, to efficiently build unique
 * vector space mappings for strings.  The methods in this trait do not mutate
 * the underlying index.  Use either a MutableIndex or one of the companion
 * object constructor methods to build an index.
 * 
 * @author dlwh, dramage
 */
trait Index[T] extends Iterable[T] with (T=>Int) {

  /** Number of elements in this index. */
  def size : Int;

  /**
   * Returns the int id of the given element (0-based) or -1 if not
   * found in the index.  This method never changes the index (even
   * in MutableIndex).
   */
  def apply(t : T) : Int;

  /**
  * Returns Some(t) if this int corresponds to some object,
  * and None otherwise.
  */
  def unapply(i : Int) : Option[T];

  /** Returns true if this index contains the element t. */
  def contains(t : T) : Boolean
    = apply(t) >= 0;

  /** Returns Some(i) if the object has been indexed, or None. */
  def indexOpt(t : T) : Option[Int] = {
    val i = apply(t);
    if (i >= 0) Some(i) else None;
  }

  /** Override Iterable's linear-scan indexOf to use our apply method. */
  def indexOf(t: T) : Int =
    apply(t);

  /** Returns the indexed items along with their indicies */
  def pairs: Iterator[(T,Int)];

  /**
   * Returns an object at the given position or throws
   * IndexOutOfBoundsException if it's not found.
   */
  def get(i : Int) : T =
    unapply(i).getOrElse(throw new IndexOutOfBoundsException());

  override def equals(other : Any) : Boolean = {
    other match {
      case that : Index[_] if this.size == that.size =>
        this sameElements that;
      case _ => false;
    }
  }

  protected lazy val defaultHashCode =
    (17 /: this)(_ * 41 + _.hashCode);

  override def hashCode = defaultHashCode;

  override def toString = {
    iterator.mkString("Index(",",",")");
  }

  def |[U](right: Index[U]) = new EitherIndex(this,right)
}

/**
 * A proxy passing all calls to the underlying index instance.
 *
 * @author dramage
 */
trait IndexProxy[T] extends Index[T] with IterableProxy[T] {
  override def self : Index[T];

  override def size = self.size;
  override def apply(t : T) = self.apply(t);
  override def unapply(i : Int) = self.unapply(i);
  override def contains(t : T) = self.contains(t);
  override def indexOpt(t : T) = self.indexOpt(t);
  override def indexOf(t : T) = self.indexOf(t);
  override def get(i : Int) = self.get(i);
  override def equals(other : Any) = self.equals(other);
  override def hashCode = self.hashCode;
  /** Returns the indexed items along with their indicies */
  def pairs: Iterator[(T,Int)] = self.pairs;
}

/**
 * Synchronized view of an Index for thread-safe access.
 *
 * @author dramage
 */
trait SynchronizedIndex[T] extends Index[T] {
  abstract override def size = this synchronized super.size;
  abstract override def apply(t : T) = this synchronized super.apply(t);
  abstract override def unapply(pos : Int) = this synchronized super.unapply(pos);
  abstract override def contains(t : T) = this synchronized super.contains(t);
  abstract override def indexOpt(t : T) = this synchronized super.indexOpt(t);
  abstract override def indexOf(t : T) = this synchronized super.indexOf(t);
  abstract override def get(pos : Int) = this synchronized super.get(pos);
  abstract override def equals(other : Any) = this synchronized super.equals(other);
  abstract override def hashCode = this synchronized super.hashCode;
}

/**
 * An Index that contains an extra method: <em>index</em> that adds the
 * given element (if necessary), returning its (possibly new) position in
 * the index.
 *
 * @author dramage
 */
trait MutableIndex[T] extends Index[T] {
  /**
   * Returns an integer index for the given object, adding it to the
   * index if it is not already present.
   */
  def index(t : T) : Int;
}

/**
 * A proxy for MutableIndex instances.
 *
 * @author dramage
 */
trait MutableIndexProxy[T] extends IndexProxy[T] with MutableIndex[T] {
  override def self : MutableIndex[T];
  override def index(t : T) = self.index(t);
}

/**
 * A synchronized view of a MutableIndex.
 *
 * @author dramage
 */
trait SynchronizedMutableIndex[T] extends MutableIndex[T] with SynchronizedIndex[T] {
  abstract override def index(t : T) = this synchronized super.index(t);
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
class HashIndex[T] extends MutableIndex[T] with Serializable {
  /** Forward map from int to object */
  private val objects = new ArrayBuffer[T];

  /** Map from object back to int index */
  private val indices = new HashMap[T,Int]();

  override def size =
    indices.size;

  override def apply(t : T) : Int =
    indices.getOrElse(t,-1);

  override def unapply(pos : Int) : Option[T] =
    if (pos >= 0 && pos < objects.length) Some(objects(pos)) else None;

  override def contains(t : T) =
    indices contains t;

  override def indexOpt(t : T): Option[Int] =
    indices.get(t);

  override def get(pos : Int) =
    objects(pos); // throws IndexOutOfBoundsException as required

  override def iterator =
    objects.iterator;

  /** Returns the position of T, adding it to the index if it's not there. */
  override def index(t: T) = {
    def nextMax = {
      val m = objects.size;
      objects += t;
      m
    }
    indices.getOrElseUpdate(t,nextMax);
  }

  def pairs = indices.iterator;
}

/**
* For use when we need an index, but we already have (densely packed) positive
* ints and don't want hash overhead.
*
* @author dlwh, dramage
*/
class DenseIntIndex(max: Int) extends Index[Int] {

  override def size = max;

  override def apply(t : Int) =
    if (contains(t)) t else -1;

  override def unapply(i : Int) =
    if (contains(i)) Some(i) else None;

  override def contains(t : Int) =
    t < max && t >= 0;

  override def indexOpt(t : Int) =
    if (contains(t)) Some(t) else None;

  override def get(i : Int) =
    if (contains(i)) i else throw new IndexOutOfBoundsException();

  override def iterator =
    (0 to max).iterator;

  def pairs = iterator zip iterator;
}

/**
 * Adds an index to a type
 */
trait Indexed[T] {
  val index: Index[T] = Index[T]();
}

trait SynchronouslyIndexed[T] extends Indexed[T] {
  override val index: Index[T] = new HashIndex[T] with SynchronizedMutableIndex[T];
}

/**
 * Utilities for manipulating and creating Index objects.
 */
object Index {
  /** Constructs an empty index. */
  import scala.reflect.ClassManifest.{Char=>MChar};
  import scala.reflect.OptManifest;
  def apply[T:OptManifest]() : MutableIndex[T] = implicitly[OptManifest[T]] match {
    case _ => new HashIndex[T]; 
  }
  
  /** Constructs an Index from some iterator. */
  def apply[T:OptManifest](iterator : Iterator[T]) : Index[T] = {
    val index = Index[T]();
    // read through all iterator now -- don't lazily defer evaluation
    for (element <- iterator) {
      index.index(element);
    }
    index;
  }

  /** Constructs an Index from some iterator. */
  def apply[T](iterable : Iterable[T]) : Index[T] = {
    val index = Index[T]();
    // read through all iterator now -- don't lazily defer evaluation
    for (element <- iterable) {
      index.index(element);
    }
    index;
  }

  /**
   * Loads a String index, one line per item with line
   * numbers (starting at 0) as the indices.
   */
  def load(source : {def getLines : Iterator[String]}) : Index[String] = {
    apply(source.getLines.map(_.stripLineEnd));
  }

  implicit object FileFormat extends scalanlp.serialization.FileSerialization.ReadWritable[Index[String]] {
    import scalanlp.pipes.Pipes.global._;
    
    override def read(source : java.io.File) = {
      Index(source.getLines);
    }

    override def write(target : java.io.File, index : Index[String]) = {
      if (index.iterator.exists(_.contains("\n"))) {
        throw new scalanlp.serialization.SerializationException("Cannot serialize index with strings that contain newline.");
      }
      index.iterator | target;
    }
  }


}

/**
 * An Index over two kinds of things. Layout is straightforward:
 * The first left.size entries are from the left index, while the next
 * right.size are from the right index. Values are wrapped in Left/Right
 *
 * @author dlwh
 */
class EitherIndex[L,R](left: Index[L], right: Index[R]) extends Index[Either[L,R]] {
  def apply(t: Either[L, R]) = t match {
    case Left(l) => left(l)
    case Right(r) => right(r) + rightOffset
  }

  /**
   * What you add to the indices from the rightIndex to get indices into this index
   * @return
   */
  def rightOffset = left.size

  def unapply(i: Int) = {
    if(i < 0 || i >= size) None
    else if(i < left.size) Some(Left(left.get(i)))
    else Some(Right(right.get(i-left.size)))
  }

  def pairs = left.pairs.map { case (l,i) => Left(l) -> i} ++ right.pairs.map { case (r,i) => Right(r) -> (i + left.size) }

  def iterator = left.iterator.map{Left(_)} ++ right.map{Right(_)}

  override def size:Int = left.size + right.size
}

/**
 * An Index over N kinds of things. A little type unsafe.
 *
 * @author dlwh
 */
class CompositeIndex[U](indices: Index[_ <:U]*) extends Index[(Int,U)] {
  private val offsets:Array[Int] = indices.unfold(0){ (n,i) => n + i.size}.toArray

  def apply(t: (Int,U)) = {
    if(t._1 >= indices.length || t._1 < 0) -1
    else {
      indices(t._1).asInstanceOf[Index[U]](t._2) + offsets(t._1)
    }
  }

  def unapply(i: Int) = {
    if(i < 0 || i >= size) None
    else {
      val index = {
        val res = Arrays.binarySearch(offsets,i)
        if(res >= 0) res
        else -(res+2)
      }

      Some(index -> indices(index).get(i-offsets(index)))
    }
  }

  def pairs = indices.iterator.zipWithIndex.flatMap { case (index,i) => index.iterator.map { t => (i,t:U)}}.zipWithIndex

  def iterator = indices.iterator.zipWithIndex.flatMap { case (index,i) => index.iterator.map{ t => (i -> t)}}

  override def size:Int = offsets.last
}
