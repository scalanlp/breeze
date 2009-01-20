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
  def nextGroup[A,B](iterA : BufferedIterator.Advanced[A], iterB : BufferedIterator.Advanced[B])(compare : ((A,B) => Int)) : (List[A],List[B]) = {
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
  def groups[A,B](iterA : Iterator[A], iterB : Iterator[B])(compare : ((A,B) => Int)) : Iterator[(List[A],List[B])] = {
    val _iterA = iterA.buffered.advanced;
    val _iterB = iterB.buffered.advanced;
    
    new Iterator[(List[A],List[B])] {
      override def hasNext = _iterA.hasNext || _iterB.hasNext;
      override def next = nextGroup(_iterA, _iterB)(compare);
    }
  }
  
  def joinInner[A,B](iterA : Iterator[A], iterB : Iterator[B])(compare : ((A,B) => Int)) : Iterator[(A,B)] = {
    for (group <- groups(iterA, iterB)(compare); a <- group._1.elements; b <- group._2.elements)
      yield (a,b);
  }
  
  /**
   * Merges (ordered) iterators by returning the lesser element at the
   * head of each, according to the given comparator.  Ties go to the element
   * from the first iterator.
   */
  def merge[T](iters : Iterator[T]*)(compare : ((T,T) => Int)) : Iterator[T] =
    new Iterator[T] {
      /** Keep track of the top of each list. */
      val heads = iters.map(get _).toArray;

      /** The merged iterator has more if any head is not None. */
      override def hasNext : Boolean =
        heads.map(_ != None).foldLeft(false)(_||_);
      
      /** Return the smallest element that is currently a list head. */
      override def next : T = {
        val top = heads.zipWithIndex.foldLeft((None.asInstanceOf[Option[T]],-1)) {
          (headA : (Option[T],Int), headB : (Option[T],Int)) =>
            (headA, headB) match {
              case ((Some(a),i), (Some(b),j)) => if (compare(a,b) <= 0) headA else headB;
              case ((Some(a),i), (None,j))    => headA;
              case ((None,i), (Some(b),j))    => headB;
              case ((None,i), (None,j))       => headA;
              case x:Any => throw new IllegalStateException(x.toString);
              }
            }
        
        // update the top list and return its value
        heads(top._2) = get(iters(top._2));
        return top._1.get;
      }
      
      def get(iter : Iterator[T]) : Option[T] =
        if (iter.hasNext) Some(iter.next) else None;
  }
  
  def main(argv : Array[String]) {
    def compare(a:Int,b:Int) = a.compare(b);
    groups(List(-1,0,1,2,3,4,6).elements, List(0,1,2,2,2,2,4,5,6).elements)(compare).foreach(println)
    println(merge(List(0,2,3).elements,List(1,1,3).elements,List(-1,5).elements)(compare).mkString(" "))
  }
}
