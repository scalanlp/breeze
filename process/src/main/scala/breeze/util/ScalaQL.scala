package breeze.util

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
  def groupBy[E,K,V](elements : Iterator[E],
                     keyF : (E=>K),
                     valF : (E=>V)) : scala.collection.mutable.Map[K,List[V]] = {
    val map = new scala.collection.mutable.HashMap[K,List[V]];
    for (elem <- elements) {
      val key = keyF(elem);
      map(key) = valF(elem) :: map.getOrElse(key, List[V]());
    }
    map;
  }

  class ReadaheadIterator[A](iter : Iterator[A]) extends Iterator[A] {
    protected val peeked = scala.collection.mutable.ListBuffer[A]();

    override def hasNext = !peeked.isEmpty || iter.hasNext;

    override def next = {
      if (!peeked.isEmpty) {
        peeked.remove(0);
      } else {
        iter.next;
      }
    }

    def peekList(n : Int) = {
      while (peeked.length < n && iter.hasNext) {
        peeked.append(iter.next);
      }
      peeked.view(0, n);
    }

    def head = {
      if (peeked.length < 1) peeked.append(iter.next);
      peeked.head;
    }
  }

  /**
   * Returns an iterator over groups of consecutive elements in iterA and iterB
   * that are equal to eachother according to the given comparator.
   */
  def groups[A,B](iteratorA : Iterator[A], iteratorB : Iterator[B])(compare : ((A,B) => Int)) : Iterator[(List[A],List[B])] = {
    val iterA = new ReadaheadIterator(iteratorA);
    val iterB = new ReadaheadIterator(iteratorB);

    new Iterator[(List[A],List[B])] {
      override def hasNext = iterA.hasNext || iterB.hasNext;
      override def next : (List[A],List[B]) = {
        if (!iterA.hasNext && !iterB.hasNext) {
          // neither iterator has more elements
          throw new NoSuchElementException("Both iterators empty");
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
        while (iterA.peekList(incA+1).length == incA+1 && compare(iterA.peekList(incA+1).last, iterB.head) == 0) {
          incA += 1;
        }

        // increment b while still matching a
        while (iterB.peekList(incB+1).length == incB+1 && compare(iterA.head, iterB.peekList(incB+1).last) == 0) {
          incB += 1;
        }

        // if have matches, return them
        if (incA > 0 || incB > 0) {
          return ((1 to incA).map((x:Int) => iterA.next).toList,
                  (1 to incB).map((x:Int) => iterB.next).toList)
        }

        // need to increment one or the other by itself
        val cc = compare(iterA.head, iterB.head);
        if (cc < 0) {
          // increment iterA
          return (List(iterA.next), List[B]());
        } else if (cc > 0) {
          // increment iterB
          return (List[A](), List(iterB.next));
        }

        throw new RuntimeException("Unexpected code branch");
      }
    }
  }

  def joinInner[A,B](iterA : Iterator[A], iterB : Iterator[B])(compare : ((A,B) => Int)) : Iterator[(A,B)] = {
    for (group <- groups(iterA, iterB)(compare); a <- group._1.iterator; b <- group._2.iterator)
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
}
