package scalanlp.collection.immutable;

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



import scala.collection._;
import mutable.{Builder, PriorityQueue}
import scala.collection.generic._;

/**
 * Represents a beam, which is essentially a priority queue
 * with a maximum size.
 * 
 * @author dlwh
 */
class Beam[T](val maxSize:Int, xs : T*)(implicit o : Ordering[T]) extends Iterable[T] with IterableLike[T,Beam[T]] { outer =>
  assert(maxSize >= 0)
  val heap = trim(BinomialHeap(xs:_*));

  override def size = heap.size;
  def this(maxSize: Int)(implicit o: Ordering[T]) = this(maxSize,Nil:_*)(o);

  private def trim(h2 : BinomialHeap[T]) = {
    var h = h2;
    while(h.size > maxSize) {
      h = h.delMin;
    }
    h
  }

  private def cat(h : BinomialHeap[T], x : T) = {
    if(h.size < maxSize) h + x;
    else if (o.compare(h.min,x) < 0) h.delMin + x;
    else h;
  }

  /** Just convert each element to the new value. Will trigger
   * a reordering.
   */
  def map[U](f: T=>U)(implicit ordering: Ordering[U]) = new Beam[U](maxSize) { 
    override val heap = BinomialHeap[U]() ++ outer.heap.map(f);
  }

  def flatMap[U](f: T=>Iterable[U])(implicit oU: Ordering[U]) = new Beam[U](maxSize) {
    override val heap = {
      val queue = new PriorityQueue[U]()(oU.reverse);
      for(x <- outer.heap;
          y <- f(x)) {
        if(queue.size < maxSize) {
          queue += y;
        } else if (oU.compare(queue.head,y) < 0) { // q.max is the smallest element.
          queue.dequeue();
          queue += y;
        }
      }
      BinomialHeap[U]() ++ queue;
    }
  }
  
  override def filter(f : T=>Boolean) = new Beam[T](maxSize) {
    override val heap = BinomialHeap[T]() ++ outer.heap.filter(f);
  }

  override protected def newBuilder = new Builder[T,Beam[T]] {
    var beam: Beam[T] = new Beam(maxSize);
    def result() = beam;

    def clear() = beam = new Beam(maxSize);

    def +=(elem: T) = {beam += elem; this}
  }


  def +(x:T) = new Beam[T](maxSize) {
    override val heap = cat(outer.heap,x);
  }
  
  def iterator = heap.iterator;
  override def toString() = iterator.mkString("Beam(",",",")");
}

object Beam {
 implicit def canBuildFrom[T<%Ordered[T]] = new CanBuildFrom[Beam[T],T,Beam[T]] {
   def apply() = error("Sorry, need a max size")

   def apply(from: Beam[T]) = from.newBuilder;
 }
}