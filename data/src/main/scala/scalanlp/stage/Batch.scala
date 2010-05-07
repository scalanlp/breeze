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
package scalanlp.stage;

import scala.reflect.Manifest;

/**
 * Represents a batch of elements that can be treated as just
 * an iterable of V.  Use the static constructors in Batch
 * to create instances.
 * 
 * @author dramage
 */
trait Batch[+V] {
  /**
   * The total number of items undelrying this batch, which is
   * greater* than or equal to the size of this.items and
   * this.values (because some items may have been filtered
   * from another underlying batch).
   */
  def size : Int;
  
  /** The smallest valid item number to return. */
  def minItemNum : Int;

  /** The largest valid item number to return. */
  def maxItemNum : Int;

  /**
   * An iterable of items ordered by their number.  The first returned
   * item.number must be >= minItemNum and the last must be <= maxItemNum.
   */
  def items : Iterable[Item[V]];

  /** An iterable of the values in this map (unboxes items). */
  def values : Iterable[V] =
    items.view.map(_.value);

  /**
   * Returns an iterable of options of values in this map of
   * length this.size.  The iterable returns Some(item.value)
   * for items present in this.items, but returns None for
   * offsets that have no corresponding item.
   */
  def options : Iterable[Option[V]] = new Iterable[Option[V]] {
    override def size = Batch.this.size;

    override def iterator = new Iterator[Option[V]] {
      var itemNum = minItemNum;
      val itemIter = Batch.this.items.iterator.buffered;
      
      override def hasNext =
        itemNum < maxItemNum;
      
      override def next : Option[V] = {
        val rv = if (itemIter.hasNext && itemIter.head.number == itemNum)
          Some(itemIter.next.value) else None;
        itemNum += 1;
        return rv;
      }
    }
  }
  
  /**
   * Transforms the items of the batch according to the given function.
   * This is non-strict by default, i.e. the computation is done again
   * each time the returned batch is iterated.
   */
  def map[O](f : V => O) : Batch[O] = {
    def mapper(item : Item[V]) = {
      try {
        item.map(f)
      } catch {
        case ex : Throwable =>
          throw new BatchException(item, ex);
      }
    }
    Batch.fromItems[O](items.view.map(mapper), size, minItemNum, maxItemNum);
  }
  
  /** Filters out items from the batch according to the given funciton. */
  def filter(f : V => Boolean) : Batch[V] = {
    def filterer(item : Item[V]) = {
      try {
        f(item.value);
      } catch {
        case ex : Throwable =>
          throw new BatchException(item, ex);
      }
    }
    Batch.fromItems(items.view.filter(filterer), size, minItemNum, maxItemNum);
  }
  
  /**
   * A view of this batch that is the same size, except only the first
   * (at most) defined n items will be return.
   */
  def take(n : Int) : Batch[V] = {
    val newMin = minItemNum;
    val newMax = Math.min(minItemNum + n, maxItemNum);
    
    Batch.fromItems(items.view.takeWhile(_.number < newMax), size, newMin, newMax);
  }

  /**
   * A view of this batch that is the same size, except that the first
   * n defined items will be dropped.
   */
  def drop(n : Int) : Batch[V] = {
    val newMin = Math.min(minItemNum + n, maxItemNum);
    val newMax = maxItemNum

    Batch.fromItems(items.view.dropWhile(_.number < newMin), size, newMin, newMax);
  }
  
  /** Zips together two batches. */
  def zip[O](that : Batch[O]) = {
    Batch.Zip(this,that).map(
      seq => (seq(0).asInstanceOf[Option[V]],
              seq(1).asInstanceOf[Option[O]]
    ));
  }
  
  /** Creates a list-backed view of this batch (i.e. makes it strict). */
  def strict : Batch[V] =
    Batch.fromItems(items.toList, size, minItemNum, maxItemNum);
}

/**
 * An exception thrown by Batch when a map or filter filter fails on
 * a given row.
 * 
 * @author dramage
 */
class BatchException(item : Item[_], cause : Throwable)
extends RuntimeException("Unable to process " + item +
                           " item " + item.number, cause);

/**
 * Static constructors for creating batches.
 */
object Batch {
  def fromItems[V](inItems : Iterable[Item[V]], numItems : Int,
                   inMinItemNum : Int, inMaxItemNum : Int) = new Batch[V] {
    override def size = numItems;
    override def minItemNum = inMinItemNum;
    override def maxItemNum = inMaxItemNum;
    override def items = inItems;
  }
  
  def fromIterable[V](inItems : Iterable[V]) = new Batch[V] {
    private lazy val cachedSize = inItems.size;
    
    override def size = cachedSize;

    override def minItemNum = 0;
    
    override def maxItemNum = size;

    override def items = new Iterable[Item[V]] {
      override def size = cachedSize;
      
      override def iterator =
        for ((v,i) <- inItems.iterator.zipWithIndex) yield Item(i, v);
    }
  }
  
  /** Zips together two batches. */
  def zip[A,B](batchA : Batch[A], batchB : Batch[B]) =
    (batchA zip batchB);
  
  /** Zips together three batches. */
  def zip[A,B,C](batchA : Batch[A], batchB : Batch[B], batchC : Batch[C])
  : Batch[(Option[A],Option[B],Option[C])] = {
    Batch.Zip(batchA,batchB,batchC).map(
      seq => (seq(0).asInstanceOf[Option[A]],
              seq(1).asInstanceOf[Option[B]],
              seq(2).asInstanceOf[Option[C]]
    ));
  }
  
  /**
   * A single batch consisting of items selected from the incoming list of batches
   * by matching on item number.  This batch iterates over Array[Option[V]] where
   * the sequence order matches the order of the batches passed as input, and
   * the next value from each batch is placed into the corresponding array cell.  If
   * a batch is missing an element, None is included in the array at that position.
   * 
   * @author dramage
   */
  class Zip[V](batches : Batch[V] *) extends Batch[Seq[Option[V]]] {
    for ((batch,i) <- batches.zipWithIndex; if batch.size != batches(0).size) {
      throw new IllegalArgumentException("Can only zip batches with the same" +
        "total number of items: batch "+i+" has "+batch.size+" != expected "+batches(0).size);
    }
    
    override def size = batches(0).size;

    override def minItemNum = batches.map(_.minItemNum).reduceLeft(Math.min);
    
    override def maxItemNum = batches.map(_.maxItemNum).reduceLeft(Math.max);

    override def items = new Iterable[Item[Seq[Option[V]]]] {
      override def size = batches(0).size;

      override def iterator = new Iterator[Item[Seq[Option[V]]]] {
        val iterators = batches.map(_.items.iterator.buffered);
      
        override def hasNext =
          iterators.exists(_.hasNext);
      
        override def next = {
          val peeks = for (iter <- iterators) yield { if (iter.hasNext) Some(iter.head) else None };
          val nextNum = peeks.filter(_.isDefined).map(_.get.number).foldLeft(Int.MaxValue)(_ min _);

          val values = (
            for ((peek, iterator) <- peeks.iterator zip iterators.iterator) yield
              if (peek.isDefined && peek.get.number == nextNum) Some(iterator.next.value) else None
          ).toList;
        
          Item(nextNum, values);
        }
      }
    }
  }

  object Zip {
    def apply[V](batches : Batch[V] *) =
      new Batch.Zip[V](batches :_*);
  }
}

/** 
 * An item represents a single item corresponding to the
 * given numbered item from the origin.
 * 
 * @param origin The (original) source of this data item.
 * @param number The number of this item in the origin.
 * @param value The value of this item.
 */
case class Item[+V](number : Int, value : V) {
  def map[O](f : V => O) =
    Item[O](number, f(value));
}


/**
 * A mapper is a stage that transforms the data from an Iterable[Option[I]] to an
 * Iterable[Option[O]] but adds no metadata.  See the MapperN variants for mappers
 * that can read metadata during the mapping process.
 * 
 * @author dramage
 */
abstract class Mapper[I,O](implicit mO : Manifest[Batch[O]])
extends Stage[Batch[I],Batch[O]] {
  /** Transforms the input data without using metadata. */
  def map(row : I) : O;

  /** Calls map. */
  override def apply(parcel : Parcel[Batch[I]]) : Parcel[Batch[O]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.map(map));
}

/**
 * Companion object to Mapper with a static constructor for simple function mappers.
 * 
 * @author dramage
 */
object Mapper {
  def apply[I,O](f : I => O)(implicit mO : Manifest[O]) = new Mapper[I,O] {
    override def map(row : I) : O = f(row);
    override def toString = "Mapper("+f.toString+")";
  }
}

/**
 * A filter is a stage that removes elements from the input Iterable.
 * If the input option is None, the output is None.  If the input is Some(x)
 * and filter(x) is false, replaces that element with None.  Otherwise it
 * reutrns Some(x) unchanged.
 */
abstract class Filter[I](implicit mI : Manifest[Batch[I]])
extends Stage[Batch[I],Batch[I]] {
  /**
   * Filters the input data without using metadata.  If the return value is
   * true, keeps the record.  If false, filters it.
   */
  def filter(row : I) : Boolean;
    
  /** Calls filter. */
  override def apply(parcel : Parcel[Batch[I]])  : Parcel[Batch[I]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.filter(filter));
}

/**
 * Companion object to Filter with a static constructor for simple function filters.
 * 
 * @author dramage
 */
object Filter {
  def apply[I](f : I => Boolean)(implicit mI : Manifest[I]) = new Filter[I] {
    override def filter(row : I) = f(row);
    override def toString = "Filter("+f.toString+")";
  }
}

/**
 * Takes the first n elements from the batch.
 * 
 * @author dramage
 */
case class Take[I](n : Int)(implicit mI : Manifest[I])
extends Stage[Batch[I],Batch[I]] {
  override def apply(parcel : Parcel[Batch[I]]) : Parcel[Batch[I]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.take(n));
  override def toString = "Take("+n+")";
}

/**
 * Drops the first n elements from the batch.
 * 
 * @author dramage
 */
case class Drop[I](n : Int)(implicit mI : Manifest[I])
extends Stage[Batch[I],Batch[I]] {
  override def apply(parcel : Parcel[Batch[I]]) : Parcel[Batch[I]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.drop(n));
  override def toString = "Drop("+n+")";
}
