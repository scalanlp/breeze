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


import scala.collection.mutable.PriorityQueue;

class Beam[T](val maxSize:Int, xs : T*)(implicit o : T=>Ordered[T]) { outer =>
  assert(maxSize >= 0)
  val heap = trim(BinomialHeap(xs:_*));

  val size = heap.size;

  private def trim(h2 : BinomialHeap[T]) = {
    var h = h2;
    while(h.size > maxSize) {
      h = h.delMin;
    }
    h
  }

  private def cat(h : BinomialHeap[T], x : T) = {
    if(h.size < maxSize) h + x;
    else if (h.min < x) h.delMin + x;
    else h;
  }

  def map[U<%Ordered[U]](f: T=>U) = new Beam[U](maxSize) { 
    override val heap = BinomialHeap[U]() ++ outer.heap.map(f);
  }

  def flatMap[U](f: T=>Collection[U])(implicit oU: U =>Ordered[U]) = new Beam[U](maxSize) {
    override val heap = {
      val queue = new PriorityQueue[U]()(reverseOrder(oU));
      for(x <- outer.heap;
          y <- f(x)) {
        if(queue.size < maxSize) {
          queue += y;
        } else if (queue.max < y) { // q.max is the smallest element.
          queue.dequeue();
          queue += y;
        }
      }
      BinomialHeap[U]() ++ queue;
    }
  }
  
  def filter(f : T=>Boolean) = new Beam[T](maxSize) {
    override val heap = BinomialHeap[T]() ++ outer.heap.filter(f);
  }


  def +(x:T) = new Beam[T](maxSize) {
    override val heap = cat(outer.heap,x);
  }
  def ++(x:Iterator[T]) : Beam[T] = new Beam[T](maxSize) {
    override val heap = x.foldLeft(outer.heap)(cat);
  }
  def ++(x:Iterable[T]) : Beam[T] = {this ++ x.elements;}

  private def reverseOrder[U](o : U=>Ordered[U]) = {x : U =>
    val oReal = o(x);
    new Ordered[U] {
      def compare(x2 : U) = -oReal.compare(x2);
    }
  }

  def elements = heap.elements;
  override def toString() = elements.mkString("Beam(",",",")");
}
