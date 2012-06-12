package breeze.util

import scala.collection.JavaConversions._;

/**
 * caches for memoized functions
 * @author dramage
 */
trait Cache[K,V] extends PartialFunction[K,V] { }

import java.lang.ref.SoftReference;

import scala.collection.mutable.Map;

/**
 * Provides a cache where both keys and values are only weakly referenced
 * allowing garbage collection of either at any time, backed by a WeakHashMap.
 * 
 * This is currently a direct port of a corresponding Java class from JavaNLP,
 * but could well be adapted to be a scala map at some point.
 * 
 * @author dramage
 */
class SoftMemCache[K,V] extends Map[K,V] with Cache[K,V] {
  
  /** cache of values */
  protected val inner =
    new java.util.HashMap[HashableSoftReference, SoftReference[Option[V]]];

  /** queue of objects to remove */
  protected val removalQueue =
    new scala.collection.mutable.Queue[HashableSoftReference];

  /** Removes all objects in the removal queue */
  protected def dequeue() = {
    while (!removalQueue.isEmpty) {
      inner.remove(removalQueue.dequeue);
    }
  }
  
  /**
   * Resolves the soft reference, returning None if the reference
   * has dissappeared or Some(value) or Some(null) depending on whether
   * null was the stored value.
   */
  private def resolve(key : K, ref : SoftReference[Option[V]]) : Option[V] = {
    val got = ref.get;
    if (ref.get == null) {
      // value has been gc'd, free key
      inner.remove(new HashableSoftReference(key));
      None
    } else {
      got match {
        case Some(value) => Some(value);
        case None        => Some(null.asInstanceOf[V]);
      }
    }
  }
  
  override def clear = {
    dequeue();
    removalQueue.clear;
    inner.clear();
  }

  override def contains(key : K) = {
    dequeue();
    inner.containsKey(new HashableSoftReference(key));
  }
  
  /**
   * Returns the value currently associated with the given key if one
   * has been set with put and not been subsequently garbage collected.
   */
  override def get(key : K) : Option[V] = {
    dequeue();
    val ref = inner.get(new HashableSoftReference(key));
    if (ref != null) {
      resolve(key, ref);
    } else {
      None;
    }
  };

  /**
   * Returns the expected size of the cache.  Note that this may over-report
   * as objects may have been garbage collected.
   */
  override def size() : Int = {
    dequeue();
    inner.size;
  }
  
  /**
   * Iterates the elements of the cache that are currently present.
   */
  override def iterator : Iterator[(K,V)] = {
    dequeue();
    for (pair <- inner.entrySet.iterator;
         k = pair.getKey.get;
         v = resolve(k, pair.getValue);
         if k != null && v != None)
      yield (k, v.asInstanceOf[Some[V]].get);
  }

  /**
   * Associates the given key with a weak reference to the given value.
   * Either key or value or both may be garbage collected at any point.
   * Returns the previously associated value or null if none was
   * associated. Value must be non-null.
   */
  override def update(key : K, value : V) : Unit = {
    dequeue();
    inner.put(new HashableSoftReference(key), new SoftReference(Some(value)));
  }


  /**
   * Removes the given pair to the map.
   */
  override def +=(k : (K,V)) : this.type = {
    dequeue();
    update(k._1,k._2);
    this
  }


  /**
   * Removes the given key from the map.
   */
  override def -=(key : K) : this.type = {
    dequeue();
    inner.remove(new HashableSoftReference(key));
    this
  }

  /**
   * A SoftReference with equality and hashcode based on the underlying
   * object.  Automatically removes itself from the containing map if the
   * reference has been gc'd.
   * 
   * @author dramage
   */
  class HashableSoftReference(ref : SoftReference[K], hash : Int) {
    def this(key : K) = this(new SoftReference(key), key.hashCode);
    
    var removing = false;
    
    def get = {
      val got = ref.get;
      if (!removing && got == null) {
        removing = true;
        SoftMemCache.this.removalQueue += this;
      }
      got;
    }
    
    override def hashCode = hash;
    
    override def equals(other : Any) = {
      if (other.isInstanceOf[HashableSoftReference]) {
        val otherref = other.asInstanceOf[HashableSoftReference];
        (this eq otherref) || (this.get == otherref.get);
      } else {
        false;
      }
    }
  }
}
