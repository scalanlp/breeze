// Counters.scala -- library for fast primitive valued counters in scala
// 
// dramage 2008
// Modified by dlwh july 2008

package scalanlp.counters;

/** Utilities for dealing with fastutil collections */
object FastUtil {
  import it.unimi.dsi.fastutil.objects._
  import it.unimi.dsi.fastutil.ints._
  
  //
  // objects
  //
  
  @inline implicit def iObjectIterator[T](iter : ObjectIterator[T]) : Iterator[T] = {
    new Iterator[T] {
      override def hasNext() : Boolean = iter.hasNext()
      override def next() : T = iter.next()
    }
  }
    
  @inline implicit def iObjectSet[T](set : ObjectSet[T]) : Iterable[T] = {
    new Iterable[T] { def elements = set.iterator() }
  }
  
  //
  // ints
  //
  
  @inline implicit def iIntIterator(iterator: IntIterator) : Iterator[Int] = {
    new Iterator[Int] {
      override def hasNext() : Boolean = iterator.hasNext()
      override def next() : Int = iterator.nextInt()
    }
  }
      
  @inline implicit def iIntCollection(jcol : IntCollection) : Iterable[Int] = {
    new Iterable[Int] { def elements = jcol.intIterator() }
  }
    
  @inline implicit def iObject2IntMapEntry[K](tup : Object2IntMap.Entry[K]) : (K,Int) = {
    (tup.getKey(), tup.getIntValue())
  }
    
  @inline implicit def iObject2IntFastIterator[K](iterator : ObjectIterator[Object2IntMap.Entry[K]]) : Iterator[(K,Int)] = {
    new Iterator[(K,Int)] {
      override def hasNext() : Boolean = iterator.hasNext()
      override def next() : (K,Int) = iterator.next()
    }
  }
    
  
  //
  // doubles
  //
  
  import it.unimi.dsi.fastutil.doubles._
 
  @inline implicit def iDoubleIterator(iterator: DoubleIterator) : Iterator[Double] = {
    new Iterator[Double] {
      override def hasNext() : Boolean = iterator.hasNext()
      override def next() : Double = iterator.nextDouble()
    }
  }
  
  @inline implicit def iDoubleList[E <: DoubleList](jcol : E) : Seq[Double] = {
    new Seq[Double] {
      def length = jcol.size
      def elements = jcol.doubleIterator()
      def apply(index : Int) = jcol.getDouble(index)
    }
  }
  
  @inline implicit def iDoubleCollection[E <: DoubleCollection](jcol : E) : Collection[Double] = {
    new Collection[Double] {
      def size = jcol.size
      def elements = jcol.doubleIterator()
    }
  }
    
  @inline implicit def iObject2DoubleMapEntry[K](tup : Object2DoubleMap.Entry[K]) : (K,Double) = {
    (tup.getKey(), tup.getDoubleValue())
  }
    
  @inline implicit def iObject2DoubleFastIterator[K](iterator : ObjectIterator[Object2DoubleMap.Entry[K]]) : Iterator[(K,Double)] = {
    new Iterator[(K,Double)] {
      override def hasNext() : Boolean = iterator.hasNext()
      override def next() : (K,Double) = iterator.next()
    }
  }
    
}

/** Utilities for counting objects, etc */
object Counters {

  /** Count the objects in the given iterator */
  def count[T](iter : Iterator[T]) : IntCounter[T] = {
    if(!iter.hasNext) return IntCounter[T]();
    // Try to use the specialized counter
    var elem = iter.next;
    val firstClass = elem.asInstanceOf[AnyRef].getClass;
    var borked = false;
    var counter = IntCounter[T](elem);
    counter(elem) += 1;
    while(iter.hasNext) {
      elem = iter.next;
      if(!borked && elem.asInstanceOf[AnyRef].getClass != firstClass) {
        val counter2 = IntCounter[T]();
        borked = true;
        for( (k,v) <- counter.elements) { counter2(k) = v};
        counter = counter2;
      }
      counter(elem) += 1;
    }
 //   counter.trim()
    return counter
  }
    
  /** Count the objects in the given Iterable */
  def count[T](iterable : Iterable[T]) : IntCounter[T] = {
    count(iterable.elements)
  }

  /** Returns a counter that sums the counts associated with each key */
  def aggregate[T](iter : Iterator[(T,Int)]) : IntCounter[T] = {
    if(!iter.hasNext) return IntCounter[T]();
    // Try to use the specialized counter
    var elem = iter.next;
    val firstClass = elem.asInstanceOf[AnyRef].getClass;
    var borked = false;
    var counter = IntCounter[T](elem._1);
    counter(elem._1) += elem._2;
    while(iter.hasNext) {
      elem = iter.next;
      if(!borked && elem.asInstanceOf[AnyRef].getClass != firstClass) {
        val counter2 = IntCounter[T]();
        borked = true;
        for( (k,v) <- counter.elements) { counter2(k) = v};
        counter = counter2;
      }
      counter(elem._1) += elem._2;
    }
 //   counter.trim()
    return counter
  }

  /** Returns a counter that sums the counts associated with each key */
  def aggregate[T](iter : Iterator[(T,Double)]) : DoubleCounter[T] = {
    if(!iter.hasNext) return DoubleCounter[T]();
    // Try to use the specialized counter
    var elem = iter.next;
    val firstClass = elem._1.asInstanceOf[AnyRef].getClass;
    var borked = false;
    var counter = DoubleCounter[T](elem._1);
    counter(elem._1) += elem._2;
    while(iter.hasNext) {
      elem = iter.next;
      if(!borked && elem.asInstanceOf[AnyRef].getClass != firstClass) {
        val counter2 = DoubleCounter[T]();
        borked = true;
        for( (k,v) <- counter.elements) { counter2(k) = v};
        counter = counter2;
      }
      counter(elem._1) += elem._2;
    }
 //   counter.trim()
    return counter
  }
 
  def aggregate[T](iterable : Iterable[(T,Double)]) : DoubleCounter[T] = aggregate(iterable.elements);
  
  //
  // Utility classes
  //
    
  /**
   * A Top-K queue keeps a list of the top K elements seen so far as ordered
   * by the given comparator.
   */
  import scala.collection.mutable.PriorityQueue
  private class TopKQueue[T](k : Int, comparator : ((T,T) => Int)) extends PriorityQueue[T]()(
        ((a : T) => {new Ordered[T] {override def compare(b : T) = -comparator(a,b)}})) {

    override def size   : Int = super.size - 1
      
    override def += (elem : T) : Unit = {
      super.+=(elem)
      if (this.size > k) {
        this.dequeue()
      }
    }
  }
  
  /**
   * Returns the top k elements in the collection by the given comparator
   */
  def topK[T](k : Int, comparator : ((T,T) => Int))(data : Iterable[T]) : Seq[T] = {
    val topk = new TopKQueue[T](k, comparator)
    topk ++= data
    return topk
  }
  
  /**
   * Pairwise min of two counters
   */
  def pairwise[T](c1 : IntCounter[T], c2 : IntCounter[T])(f : ((Int,Int)=>Int)) : IntCounter[T] = {
    val rv = IntCounter[T]()
    
    for ((k,v) <- c1.elements) {
      rv.put(k, f(v,c2(k)))
    }
    for ((k,v) <- c2.elements) {
      if (!rv.contains(k))
        rv.put(k, f(c1(k),v))
    }
    
    return rv
  }
}
