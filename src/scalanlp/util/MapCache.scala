package scalanlp.util

import java.lang.ref.SoftReference;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import JavaCollections._;

/**
 * Provides a cache where both keys and values are only weakly referenced
 * allowing garbage collection of either at any time, backed by a WeakHashMap.
 * 
 * This is currently a direct port of a corresponding Java class from JavaNLP,
 * but could well be adapted to be a scala map at some point.
 * 
 * @author dramage
 */
class MapCache[K,V] extends AbstractMap[K,V] {
  
  def getOrNull[V](reference : SoftReference[V]) : V = {
    if (reference != null) reference.get() else null.asInstanceOf[V];
  }
  
  /** cache of values */
  private val map = new WeakHashMap[K, SoftReference[V]]();

  /** Clears this map. */
  override def clear = map.clear();

  /**
   * Returns true if a non-null value is associated with the given key
   * (and has not yet been garbage collected).
   */
  override def containsKey(key : AnyRef) : Boolean =
    getOrNull(map.get(key)) != null;

  /**
   * Returns true if the given non-null value is present in the map.
   */
  override def containsValue(value : AnyRef) : Boolean = {
    if (value == null) {
      throw new IllegalArgumentException("SoftCache does not support null values");
    }
    
    for (valRef <- map.values()) {
      if (valRef.get() != null && value.equals(valRef.get())) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns the value currently associated with the given key if one
   * has been set with put and not been subsequently garbage collected.
   */
  override def get(key : Any) : V = getOrNull(map.get(key));

  /**
   * Returns true if the map is empty.  Note that some values may have
   * been garbage collected resulting in effectively empty maps still
   * returning true.
   */
  override def isEmpty() = map.isEmpty();

  /**
   * Returns the set of keys currently in this map -- some keys
   * may reference values that have been garbage collected, so
   * callers should be sure to re-check the value of a call to get.
   */
  override def keySet() = map.keySet();

  /**
   * Associates the given key with a weak reference to the given value.
   * Either key or value or both may be garbage collected at any point.
   * Returns the previously associated value or null if none was
   * associated. Value must be non-null.
   */
  override def put(key : K, value : V) : V = {
    if (value == null) {
      throw new IllegalArgumentException("WeakCache does not support null values");
    }
    return getOrNull(map.put(key, new SoftReference[V](value)));
  }

  /**
   * Removes the given key from the map.
   */
  override def remove(key : Any) : V = getOrNull(map.remove(key));

  /**
   * Returns the expected size of the cache.  Note that this may over-report
   * as objects may have been garbage collected.
   */
  override def size() : Int = map.size;
	
  /**
   * Returns an immutable set of entries backed by this cache.  Attempts
   * to modify or remove values in this map will fail.
   */
  override def entrySet() : Set[Map.Entry[K,V]] = {
    return new AbstractSet[Map.Entry[K,V]]() {
      override def iterator() : Iterator[Map.Entry[K,V]] = {
        return new Iterator[Map.Entry[K,V]]() {
          val _iter = map.entrySet().iterator();
          var _next = prepare();
          
          override def hasNext() = _next != null;

          override def next() : Map.Entry[K,V] = {
            val rv = _next;
            _next = prepare();
            return rv;
          }

          // we would need to call iterator.remove() on the *previous* entry
          // in iterator, because iterator has already advanced in prepare()
          override def remove() =
            throw new UnsupportedOperationException("Cannot remove from" +
            		" iterator on a SoftCache because of map consistency issues.");
          
          def prepare() : Map.Entry[K,V] = {
            while (_iter.hasNext) {
              val ref = _iter.next();
              val value : V = getOrNull(ref.getValue());
              if (value != null) {
                return new Map.Entry[K,V]() {
                  override def getKey = ref.getKey;
                  override def getValue = value;
                  override def setValue(value : V) =
                    getOrNull(ref.setValue(new SoftReference[V](value)));
                  }
                }
              }
            return null;
          }
        };
      }

      /**
       * Returns a possibly tight (over-)estimate of the number of entries
       * in the map.
       */
      override def size() = map.size();
    };
  }
}
