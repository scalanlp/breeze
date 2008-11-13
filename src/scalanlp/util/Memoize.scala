package scalanlp.util

/**
 * Functions that return a memoized version of an n-arg function.
 * All function arguments are assumed to be hashable.  The cached
 * values are stored in an underlying MapCache, which will use
 * available memory but will not prevent the JVM from garbage
 * collecting the references.
 * 
 * @author dramage
 */
object Memoize {
  /** Function that creates a new cache */
  private def createCache[K,V]() =
    scala.collection.jcl.Conversions.convertMap(new MapCache[K,Option[V]]());
  
  /**
   * Returns a memoized version of the given 1-arg input function,
   * backed by a MapCache.
   */
  def memoize[A1,V](func : ((A1) => V)) : ((A1) => V) = {
    val cache = createCache[A1,V]();
    
    // returns a fresh version of the given function call, storing
    // an option for its return value in the cache.
    def fresh(arg1:A1) : V = {
      val value = func(arg1);
      cache.put(arg1, if (value == null) None else Some(value));
      value;
    }
    
    // returns a function that checks the cache
    return ((arg1:A1) => {
      if (cache.contains(arg1)) {
        val cached = cache(arg1);
        if (cached == null) {
          fresh(arg1);
        } else {
          cached match {
            case Some(x) => x;
            case None => null.asInstanceOf[V];
          }
        }
      } else {
        fresh(arg1);
      }
    });
  }
  
  /**
   * Returns a memoized version of the given 2-arg input function,
   * backed by a MapCache.
   */
  def memoize[A1,A2,V](func : ((A1,A2) => V)) : ((A1,A2) => V) = {
    val cache = createCache[(A1,A2),V]();

    // returns a fresh version of the given function call, storing
    // an option for its return value in the cache.
    def fresh(arg1:A1, arg2:A2) : V = {
      val value = func(arg1, arg2);
      cache.put((arg1,arg2), if (value == null) None else Some(value));
      value;
    }
    
    // returns a function that checks the cache
    return ((arg1:A1, arg2:A2) => {
      if (cache.contains((arg1,arg2))) {
        val cached = cache((arg1,arg2));
        if (cached == null) {
          fresh(arg1,arg2);
        } else {
          cached match {
            case Some(x) => x;
            case None => null.asInstanceOf[V];
          }
        }
      } else {
        fresh(arg1,arg2);
      }
    });
  }

  //
  // memoized versions of the memoize functions.
  //
  
//  private val memoize1 = _memoize1(_memoize1 _);
//  private val memoize2 = _memoize1(_memoize2 _);
  
  /**
   * Returns a memoized version of the given 1-arg input function,
   * backed by a MapCache.  This function is itself memoized, so
   * repeated calls with the same function will return exactly the
   * same value subject to garbage collection oddities.
   */
//  def memoize[A1,V](func : ((A1) => V)) : ((A1) => V) =
//    memoize1(func.asInstanceOf[(Nothing) => Nothing]).asInstanceOf[(A1)=>V];
  
  /**
   * Returns a memoized version of the given 2-arg input function,
   * backed by a MapCache.  This function is itself memoized, so
   * repeated calls with the same function will return exactly the
   * same value subject to garbage collection oddities.
   */
//  def memoize[A1,A2,V](func : ((A1,A2) => V)) : ((A1,A2) => V) =
//    memoize2(func.asInstanceOf[(Nothing,Nothing) => Nothing]).asInstanceOf[(A1,A2)=>V];
}

object MemoizeTest {
  import Memoize._
  
  def fact(x : Long) : Long = {
    println(x+"!");
    if (x == 0) 1 else x * fact(x-1);
  }
  
  val mfact = memoize(fact _);
  
  def main(args : Array[String]) {
    println(fact(3));
    println(fact(3));
    println(mfact eq memoize(fact _))
    
    println(mfact(5));
    println(mfact(5));
  }
}
