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
  def groupBy[E,K](seq : Seq[E], keyF : (E=>K))  : scala.collection.mutable.Map[K,List[E]] =
    groupBy(seq.iterator, keyF, (a:E) => a);
  
  /** Groups the elements of the given sequence into a map by the given key function. */
  def groupBy[E,K,V](iterator : Iterator[E],
                     keyF : (E=>K),
                     valF : (E=>V)) : scala.collection.mutable.Map[K,List[V]] = {
    val map = new scala.collection.mutable.HashMap[K,List[V]];
    for (elem <- iterator) {
      val key = keyF(elem);
      map(key) = valF(elem) :: map.getOrElse(key, List[V]());
    }
    map;
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
  
}
