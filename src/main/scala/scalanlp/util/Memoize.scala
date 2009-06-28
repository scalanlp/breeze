package scalanlp.util

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
    new MapCache[K,V]();
  
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
      cache.put(arg1, value);
      value;
    }
    
    // returns a function that checks the cache
    return ((arg1:A1) => {
      cache.get(arg1) match {
        case Some(x) => x;
        case None => fresh(arg1);
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
      cache.put((arg1,arg2), value);
      value;
    }
    
    // returns a function that checks the cache
    return ((arg1:A1, arg2:A2) => {
      cache.get(arg1,arg2) match {
        case Some(x) => x;
        case None => fresh(arg1,arg2);
      }
    });
  }

  /**
   * Returns a memoized version of the given 3-arg input function,
   * backed by a MapCache.
   */
  def memoize[A1,A2,A3,V](func : ((A1,A2,A3) => V)) : ((A1,A2,A3) => V) = {
    val cache = createCache[(A1,A2,A3),V]();
    // returns a fresh version of the given function call, storing
    // an option for its return value in the cache.
    def fresh(arg1:A1, arg2:A2, arg3:A3) : V = {
      val value = func(arg1, arg2, arg3);
      cache.put((arg1,arg2,arg3), value);
      value;
    }
    
    // returns a function that checks the cache
    return ((arg1:A1, arg2:A2, arg3:A3) => {
      cache.get(arg1,arg2,arg3) match {
        case Some(x) => x;
        case None => fresh(arg1,arg2,arg3);
      }
    });
  }

  //
  // memoized versions of the memoize functions.
  //
  
  //private val memoize1 = _memoize1(_memoize1 _);
  //private val memoize2 = _memoize1(_memoize2 _);
  
  /**
   * Returns a memoized version of the given 1-arg input function,
   * backed by a MapCache.  This function is itself memoized, so
   * repeated calls with the same function will return exactly the
   * same value subject to garbage collection oddities.
   */
  //def memoize[A1,V](func : ((A1) => V)) : ((A1) => V) =
  //  memoize1(func.asInstanceOf[(Nothing) => Nothing]).asInstanceOf[(A1)=>V];
  
  /**
   * Returns a memoized version of the given 2-arg input function,
   * backed by a MapCache.  This function is itself memoized, so
   * repeated calls with the same function will return exactly the
   * same value subject to garbage collection oddities.
   */
  //def memoize[A1,A2,V](func : ((A1,A2) => V)) : ((A1,A2) => V) =
  //  memoize2(func.asInstanceOf[(Nothing,Nothing) => Nothing]).asInstanceOf[(A1,A2)=>V];
}

object MemoizeTest {
  import Memoize._
  
  var evaluations = 0;
    
  def ff(in : String) = { evaluations += 1; in.toUpperCase; }
  val mf = memoize(ff _);
  
  def main(args : Array[String]) {
    val r = new java.util.Random;
    
    def evaluate(N : Int, M : Int) = {
      evaluations = 0;
      for (i <- 1 to N) { mf("ab"+r.nextInt(M).toString); }
      println("Evaluated " + evaluations + " / " + N);
    }
    
    evaluate(1000000,200000);
  }
}
