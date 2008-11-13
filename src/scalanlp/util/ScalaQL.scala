package scalanlp.util

/** 
* Database-like operations on iterators 
* 
* @author(dramage)
*/
object ScalaQL {
  
  /** Groups the elements of the given sequence into a map by the given key function. */
  def groupBy[E,K](seq : Seq[E], keyF : (E=>K)) : scala.collection.Map[K,List[E]] = {
    val map = new scala.collection.mutable.HashMap[K,List[E]];
    for (elem <- seq) {
      val key = keyF(elem);
      map(key) = elem :: map.getOrElse(key, List[E]());
    }
    map;
  }
  
  /**
   * Returns the next group of elements from A and B that are all equivalent
   * with eachother.
   */
  def nextGroup[A,B](iterA : BufferedIterator.Advanced[A], iterB : BufferedIterator.Advanced[B], compare : ((A,B) => Int)) : (List[A],List[B]) = {
    if (!iterA.hasNext && !iterB.hasNext) {
      // neither iterator has more elements
      throw new IllegalAccessException("Both iterators empty");
    } else if (iterA.hasNext && !iterB.hasNext) {
      // just iterA
      return (List(iterA.next),List[B]())
    } else if (!iterA.hasNext && iterB.hasNext) {
      // just iterB
      return (List[A](),List(iterB.next))
    }
    
    // take matching elements off top of both iterators
    var incA = 0
    var incB = 0
    
    // increment a while still matching b
    while (iterA.peekList(incA+1).length == incA+1 && compare(iterA.peekList(incA+1).last, iterB.peek(0)) == 0) {
      incA += 1;
    }
    
    // increment b while still matching a
    while (iterB.peekList(incB+1).length == incB+1 && compare(iterA.peek(0), iterB.peekList(incB+1).last) == 0) {
      incB += 1;
    }
    
    // if have matches, return them
    if (incA > 0 || incB > 0) {
      return ((1 to incA).map((x:Int) => iterA.next).toList,
              (1 to incB).map((x:Int) => iterB.next).toList)
    }
    
    // need to increment one or the other by itself
    val cc = compare(iterA.peek(0), iterB.peek(0));
    if (cc < 0) {
      // increment iterA
      return (List(iterA.next), List[B]());
    } else if (cc > 0) {
      // increment iterB
      return (List[A](), List(iterB.next));
    }
    
    throw new RuntimeException("Unexpected code branch");
  }
  
  /**
   * Returns an iterator over groups of consecutive elements in iterA and iterB
   * that are equal to eachother according to the given comparator.
   */
  def groups[A,B](iterA : Iterator[A], iterB : Iterator[B], compare : ((A,B) => Int)) : Iterator[(List[A],List[B])] = {
    val _iterA = iterA.buffered.advanced;
    val _iterB = iterB.buffered.advanced;
    
    new Iterator[(List[A],List[B])] {
      override def hasNext = _iterA.hasNext || _iterB.hasNext;
      override def next = nextGroup(_iterA, _iterB, compare);
    }
  }
  
  def joinInner[A,B](iterA : Iterator[A], iterB : Iterator[B], compare : ((A,B) => Int)) : Iterator[(A,B)] = {
    for (group <- groups(iterA, iterB, compare); a <- group._1.elements; b <- group._2.elements)
      yield (a,b);
  }
  
  def main(argv : Array[String]) {
    def compare(a:Int,b:Int) = a.compare(b);
    groups(List(-1,0,1,2,3,4,6).elements, List(0,1,2,2,2,2,4,5,6).elements, compare).foreach(println)
  }
}
